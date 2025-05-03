use std::{
    collections::{BTreeMap, BTreeSet},
    fs::File,
    io::{BufRead, BufReader, read_to_string, stdin},
    path::Path,
};

use clap::Parser;
use tzif::{
    data::{
        time::Seconds,
        tzif::{DataBlock, LocalTimeTypeRecord, TzifHeader},
    },
    parse_tzif_file,
};

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    version: String,

    #[arg(short, long)]
    data_dir: String,

    #[arg(short, long, default_value = "zone1970.tab")]
    zone_file_name: String,

    #[arg(short, long, default_value = "tzdata.zi")]
    tzdata_file_name: String,
}

fn main() {
    let Args {
        version,
        data_dir,
        zone_file_name,
        tzdata_file_name,
    } = Args::parse();

    let template = read_to_string(stdin()).expect("couldn't read template from stdin");
    let zone_names = read_zone_names(format!("{data_dir}/{zone_file_name}"));
    let zone_alias_names = read_zone_alias_names(format!("{data_dir}/{tzdata_file_name}"));
    let zone_designations = read_zone_designations(data_dir, zone_names);

    print_module(template, version, &zone_designations);
    print_zone_name_function(zone_alias_names, &zone_designations);

    let mut cache = BTreeMap::new();
    for (zone_name, offset_designations) in zone_designations {
        print_zone_function(zone_name, offset_designations, &mut cache)
    }
}

// READ

fn read_zone_names(path: String) -> BTreeSet<String> {
    let mut zone_names = BTreeSet::new();
    for line in BufReader::new(File::open(path).expect("couldn't open zone names file")).lines() {
        let line = line.expect("couldn't read zone names file line");
        if line.starts_with("#") {
            continue;
        }
        assert!(
            zone_names.insert(
                line.split_whitespace()
                    .nth(2)
                    .expect("missing zone name in zones file line")
                    .to_string(),
            ),
            "zone names must be unique"
        );
    }
    zone_names
}

fn read_zone_alias_names(path: String) -> BTreeMap<String, String> {
    let mut zone_alias_names = BTreeMap::new();
    for line in BufReader::new(File::open(path).expect("couldn't open tzdata file")).lines() {
        let line = line.expect("couldn't read tzdata file line");
        let mut columns = line.split_whitespace();
        if columns.next() != Some("L") {
            continue;
        }
        let zone_name = columns
            .next()
            .expect("missing zone name in tzdata file line L")
            .to_string();
        let zone_alias_name = columns
            .next()
            .expect("missing zone alias name in tzdata file line L")
            .to_string();
        assert!(
            zone_alias_names
                .insert(zone_alias_name, zone_name)
                .is_none(),
            "zone alias names must be unique"
        );
    }
    zone_alias_names
}

fn read_zone_designations(
    path: String,
    zone_names: BTreeSet<String>,
) -> BTreeMap<String, OffsetDesignations> {
    let mut zone_designations = BTreeMap::new();
    for zone_name in zone_names {
        let data_path = format!("{path}/{zone_name}");
        let zone_data =
            parse_tzif_file(Path::new(&data_path)).expect("couldn't read the TZif file");
        let offset_designations = OffsetDesignations::from_tzif_data(
            zone_data.header2.unwrap_or(zone_data.header1),
            zone_data.data_block2.unwrap_or(zone_data.data_block1),
        );
        assert!(
            zone_designations
                .insert(zone_name, offset_designations)
                .is_none(),
            "zone names must be unique"
        )
    }
    zone_designations
}

// PRINT

const DB_VERSION_PLACEHOLDER: &str = "DB_VERSION";
const ZONE_FUNCTIONS_PLACEHOLDER: &str = "ZONE_FUNCTIONS";
fn print_module(
    template: String,
    version: String,
    zone_designations: &BTreeMap<String, OffsetDesignations>,
) {
    assert!(
        template.contains(DB_VERSION_PLACEHOLDER),
        "missing database version placeholder in template"
    );
    assert!(
        template.contains(ZONE_FUNCTIONS_PLACEHOLDER),
        "missing zone functions placeholder in template"
    );
    let mut function_names = Vec::new();
    for zone_name in zone_designations.keys() {
        function_names.push(to_function_name(zone_name))
    }
    print!(
        "{}",
        template
            .replace(DB_VERSION_PLACEHOLDER, &version)
            .replace(ZONE_FUNCTIONS_PLACEHOLDER, &function_names.join(", "))
    );
}

fn print_zone_name_function(
    zone_alias_names: BTreeMap<String, String>,
    zone_designations: &BTreeMap<String, OffsetDesignations>,
) {
    println!(
        r#"
forZoneName : String -> Zone -> Posix -> Result Error Abbreviation
forZoneName name =
    let withError = ((<<) << (<<)) (Result.mapError (UnknownZoneOffset (Time.Name name)))
    in
    case name of
"#
    );
    for zone_name in zone_designations.keys() {
        println!(
            "        \"{zone_name}\" -> withError {}",
            to_function_name(zone_name)
        );
    }
    for (zone_alias_name, zone_name) in zone_alias_names {
        if !zone_designations.contains_key(&zone_name) {
            continue;
        }
        println!(
            "        \"{zone_alias_name}\" -> withError {}",
            to_function_name(&zone_name)
        );
    }
    println!("        other -> \\_ _ -> Err (UnknownZoneName other)");
}

fn print_zone_function(
    zone_name: String,
    offset_designations: OffsetDesignations,
    cache: &mut BTreeMap<OffsetDesignations, String>,
) {
    let function_name = to_function_name(&zone_name);
    println!("{{-| `{zone_name}` -}}");
    println!("{function_name} : Zone -> Posix -> Result Int Abbreviation");
    match cache.get(&offset_designations) {
        None => {
            // Optimization to group together offsets with similar designations content.
            // Likely to happen because of using two possible seconds-to-minutes offset approximations.
            let mut designation_offsets: BTreeMap<&Designations, Vec<i64>> = BTreeMap::new();
            for (offset_minutes, designations) in &offset_designations.0 {
                designation_offsets
                    .entry(designations)
                    .or_default()
                    .push(*offset_minutes);
            }
            print!("{function_name} = Internal.toTime (\\time -> ");
            for (designations, offsets_minutes) in designation_offsets {
                assert!(
                    !offsets_minutes.is_empty(),
                    "only added if at least one offset"
                );
                print!("if ");
                for (i, offset_minutes) in offsets_minutes.iter().enumerate() {
                    if i != 0 {
                        print!(" || ")
                    }
                    // This doesn't get directly translated to a JS `==` when `offsetMinutes` is negative.
                    // But avoiding it doesn't seem to have a great impact on size once minified.
                    print!("time.offsetMinutes == {offset_minutes}");
                }
                print!(" then Ok (");
                for (transition, designation) in designations.transitions.iter().rev() {
                    // This also doesn't get directly translated to a JS `>=` when `posixSeconds` is negative.
                    // But avoiding it also doesn't seem to have a great impact on size once minified.
                    print!(
                        "if time.posixSeconds >= {transition} then {} else ",
                        to_designation_value(designation)
                    );
                }
                print!("{}) else ", to_designation_value(&designations.initial));
            }
            println!("Err time.offsetMinutes)");
            assert!(
                cache.insert(offset_designations, zone_name).is_none(),
                "should not be already cached"
            )
        }
        Some(zone_name) => {
            // Optimization to reuse generated code for zones with similar designations content.
            // More likely to happen if we start providing less data (e.g. since some year or excluding LMTs).
            println!("{function_name} = {}", to_function_name(zone_name))
        }
    }
}

/// Convert from a time zone name (e.g. Europe/London) to a function name to use in the generated code.
fn to_function_name(zone_name: &str) -> String {
    format!(
        "for__{}",
        zone_name
            .replace("-", "_")
            .replace("/", "__")
            .to_lowercase()
    )
}

fn to_designation_value(designation: &Designation) -> String {
    match designation {
        Designation::Lmt => "Lmt".to_string(),
        Designation::Uninhabited => "Uninhabited".to_string(),
        Designation::Offset => "Offset time.offsetMinutes".to_string(),
        Designation::ShortName(short_name) => format!("ShortName \"{short_name}\""),
    }
}

// PARSE

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Designation {
    Lmt,
    Offset,
    Uninhabited,
    ShortName(String),
}

impl Designation {
    fn new(designation: &str, offset_minutes: i64) -> Self {
        if designation == "LMT" {
            Self::Lmt
        } else if designation == "-00" {
            Self::Uninhabited
        } else if is_offset_designation(designation, offset_minutes) {
            assert!(
                offset_minutes.abs() <= 4320,
                "offsets bigger than three days are not supported"
            );
            Self::Offset
        } else {
            assert!(
                designation.contains(char::is_alphabetic),
                "short name designations should contain at least one alphabetic character"
            );
            Self::ShortName(designation.to_string())
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Designations {
    initial: Designation,
    transitions: Vec<(i64, Designation)>,
}

impl Designations {
    fn new(initial: &str, offset_minutes: i64) -> Self {
        Self {
            initial: Designation::new(initial, offset_minutes),
            transitions: Vec::new(),
        }
    }

    /// Optimization to remove transitions to the same designation within the same offset.
    /// The information that it removes is not important if we rely on correct offsets being asked for.
    fn compress(&mut self) {
        let mut current = &self.initial;
        let mut compressed_transitions = Vec::with_capacity(self.transitions.len());
        for (transition, designation) in &self.transitions {
            if designation == current {
                continue;
            }
            current = designation;
            compressed_transitions.push((*transition, designation.clone()));
        }
        self.transitions = compressed_transitions
    }
}

#[derive(Default, PartialEq, Eq, PartialOrd, Ord)]
struct OffsetDesignations(BTreeMap<i64, Designations>);

impl OffsetDesignations {
    fn from_tzif_data(header: TzifHeader, data: DataBlock) -> Self {
        // Add initial local time designation.
        let mut offset_designations = Self::default();
        let LocalTimeTypeRecord {
            utoff: Seconds(offset_seconds),
            idx,
            ..
        } = data.local_time_type_records[0];
        let designation = data
            .time_zone_designation(idx)
            .expect("invalid initial designation index");

        offset_designations.add(designation, offset_seconds, None);

        // Add each local time designation transition by offset.
        for i in 0..header.timecnt {
            let LocalTimeTypeRecord {
                utoff: Seconds(offset_seconds),
                idx,
                ..
            } = data.local_time_type_records[data.transition_types[i]];
            let Seconds(transition_seconds) = data.transition_times[i];
            let designation = data
                .time_zone_designation(idx)
                .expect("invalid transition designation index");

            offset_designations.add(designation, offset_seconds, Some(transition_seconds));
        }

        // Compress consecutive designations within each offset.
        for designations in offset_designations.0.values_mut() {
            designations.compress();
        }

        offset_designations
    }

    fn add(&mut self, designation: &str, offset_seconds: i64, transition_seconds: Option<i64>) {
        self.add_by_minutes(designation, offset_seconds / 60, transition_seconds);

        // Elm's `Time.Zone` deals with offset in minutes, so using both possible approximations just in case.
        if offset_seconds % 60 != 0 {
            self.add_by_minutes(
                designation,
                (offset_seconds / 60) + (offset_seconds % 60).signum(),
                transition_seconds,
            );
        }
    }

    fn add_by_minutes(
        &mut self,
        designation: &str,
        offset_minutes: i64,
        transition_seconds: Option<i64>,
    ) {
        match self.0.get_mut(&offset_minutes) {
            None => {
                assert!(
                    self.0
                        .insert(
                            offset_minutes,
                            Designations::new(designation, offset_minutes)
                        )
                        .is_none(),
                    "not the initial designation for this offset"
                );
            }
            Some(Designations { transitions, .. }) => transitions.push((
                transition_seconds.expect("should be a designation transition"),
                Designation::new(designation, offset_minutes),
            )),
        }
    }
}

/// Check whether a designation corresponds to just the formatted offset.
fn is_offset_designation(designation: &str, offset_minutes: i64) -> bool {
    if offset_minutes == 0 {
        return designation == "+00";
    }

    let sign = if offset_minutes > 0 { "+" } else { "-" };
    if offset_minutes % 60 == 0 {
        designation == format!("{sign}{:02}", offset_minutes.abs() / 60)
    } else {
        designation
            == format!(
                "{sign}{:02}{:02}",
                offset_minutes.abs() / 60,
                offset_minutes.abs() % 60
            )
    }
}
