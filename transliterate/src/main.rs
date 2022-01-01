use std::{collections::HashMap, fs, path::PathBuf};

use clap::Parser;
use serde::Deserialize;
use serde_yaml;
use unicode_normalization::UnicodeNormalization;

#[derive(Debug, Deserialize, Clone)]
struct Script {
    name: String,
    style: Style,
    #[serde(default)]
    virama: String,
    vowels: Vec<Vec<String>>,
    #[serde(default)]
    diacritics: Vec<Vec<String>>,
    modifiers: Vec<Vec<String>>,
    consonants: Vec<Vec<String>>,
    punctuation: Vec<Vec<String>>,
    digits: Vec<String>,
}

#[derive(Debug, Deserialize, Copy, Clone, PartialEq, Eq)]
enum Style {
    #[serde(rename = "abugida")]
    Abugida,
    #[serde(rename = "alphabet")]
    Alphabet,
}

#[derive(Debug, Parser)]
#[clap(about, version, author)]
struct Args {
    #[clap(short, long)]
    input: PathBuf,
    #[clap(short, long)]
    output: PathBuf,
}

#[derive(Debug, Clone)]
struct ParsedScript {
    name: String,
    style: Style,
    virama: String,
    vowels: Vec<String>,
    diacritics: Vec<String>,
    modifiers: Vec<String>,
    consonants: Vec<String>,
    punctuation: Vec<String>,
    digits: Vec<String>,
}

impl Into<ParsedScript> for Script {
    fn into(self) -> ParsedScript {
        ParsedScript {
            name: self.name,
            style: self.style,
            virama: self.virama,
            vowels: self.vowels.into_iter().flatten().collect(),
            diacritics: self.diacritics.into_iter().flatten().collect(),
            modifiers: self.modifiers.into_iter().flatten().collect(),
            consonants: self.consonants.into_iter().flatten().collect(),
            punctuation: self.punctuation.into_iter().flatten().collect(),
            digits: self.digits,
        }
    }
}

fn main() {
    let args = Args::parse();
    let input = fs::read_to_string(args.input).expect("Unable to read file.");
    let mut scripts: Vec<ParsedScript> = Vec::new();
    for document in serde_yaml::Deserializer::from_str(&input) {
        let value = Script::deserialize(document).expect("Unable to deserialize.");
        scripts.push(value.into());
    }
    let (source, targets) = scripts
        .split_first()
        .expect("Could not find target scripts.");
    let mut conversions: HashMap<String, HashMap<String, String>> = HashMap::new();
    for target in targets {
        conversions.insert(
            format!("to_{}", target.clone().name),
            make_mapping(source.clone(), target.clone()),
        );
    }
    let conversions = serde_json::to_string_pretty(&conversions).expect("Unable to serialize.");
    let conversions = format!("let mapping = {};", conversions);
    fs::write(args.output, conversions).expect("Could not write to file.");
}

fn make_mapping(source: ParsedScript, target: ParsedScript) -> HashMap<String, String> {
    match (source.style, target.style) {
        (Style::Abugida, Style::Abugida) => map_abuguidas(source, target),
        (Style::Abugida, Style::Alphabet) => map_abuguida_alphabet(source, target),
        (Style::Alphabet, Style::Abugida) => map_alphabet_abugida(source, target),
        (Style::Alphabet, Style::Alphabet) => map_alphabets(source, target),
    }
}

fn map_abuguidas(source: ParsedScript, target: ParsedScript) -> HashMap<String, String> {
    assert!(source.style == Style::Abugida && target.style == Style::Abugida);
    let mut map = HashMap::new();
    for (source_vowel, target_vowel) in source.vowels.iter().zip(target.vowels) {
        map.insert(source_vowel.into(), target_vowel.into());
    }
    for (source_consonant, target_consonant) in source.consonants.iter().zip(target.consonants) {
        for (source_vowel, target_vowel) in source.diacritics.iter().zip(target.diacritics.clone())
        {
            for (source_nasal, target_nasal) in
                source.modifiers.iter().zip(target.modifiers.clone())
            {
                let s = format!("{}{}{}", source_consonant, source_vowel, source_nasal);
                let t = format!("{}{}{}", target_consonant, target_vowel, target_nasal);
                map.insert(s, t);
            }
        }
        let s = format!("{}{}", source_consonant, source.virama);
        let t = format!("{}{}", target_consonant, target.virama);
        map.insert(s, t);
    }
    for (source_punctuation, target_punctuation) in
        source.punctuation.iter().zip(target.punctuation)
    {
        map.insert(source_punctuation.into(), target_punctuation.into());
    }
    for (source_digit, target_digit) in source.digits.iter().zip(target.digits) {
        map.insert(source_digit.into(), target_digit.into());
    }
    map.into_iter()
        .map(|(k, v)| (normalize(&k), normalize(&v)))
        .collect()
}

fn map_abuguida_alphabet(source: ParsedScript, target: ParsedScript) -> HashMap<String, String> {
    assert!(source.style == Style::Abugida && target.style == Style::Alphabet);
    let mut map = HashMap::new();
    for (source_vowel, target_vowel) in source.vowels.iter().zip(target.vowels.clone()) {
        map.insert(source_vowel.into(), target_vowel.into());
    }
    for (source_consonant, target_consonant) in source.consonants.iter().zip(target.consonants) {
        for (source_vowel, target_vowel) in source.diacritics.iter().zip(target.vowels.clone()) {
            for (source_nasal, target_nasal) in
                source.modifiers.iter().zip(target.modifiers.clone())
            {
                let s = format!("{}{}{}", source_consonant, source_vowel, source_nasal);
                let t = format!("{}{}{}", target_consonant, target_vowel, target_nasal);
                map.insert(s, t);
            }
        }
        let s = format!("{}{}", source_consonant, source.virama);
        map.insert(s, target_consonant.clone());
    }
    for (source_punctuation, target_punctuation) in
        source.punctuation.iter().zip(target.punctuation)
    {
        map.insert(source_punctuation.into(), target_punctuation.into());
    }
    for (source_digit, target_digit) in source.digits.iter().zip(target.digits) {
        map.insert(source_digit.into(), target_digit.into());
    }
    map.into_iter()
        .map(|(k, v)| (normalize(&k), normalize(&v)))
        .collect()
}

fn map_alphabet_abugida(source: ParsedScript, target: ParsedScript) -> HashMap<String, String> {
    assert!(source.style == Style::Alphabet && target.style == Style::Abugida);
    let mut map = HashMap::new();
    for (source_vowel, target_vowel) in source.vowels.iter().zip(target.vowels.clone()) {
        map.insert(source_vowel.into(), target_vowel.into());
    }
    for (source_consonant, target_consonant) in source.consonants.iter().zip(target.consonants) {
        for (source_vowel, target_vowel) in source.vowels.iter().zip(target.diacritics.clone()) {
            for (source_nasal, target_nasal) in
                source.modifiers.iter().zip(target.modifiers.clone())
            {
                let s = format!("{}{}{}", source_consonant, source_vowel, source_nasal);
                let t = format!("{}{}{}", target_consonant, target_vowel, target_nasal);
                println!("{}, {}", s, t);
                map.insert(s, t);
            }
        }
        let t = format!("{}{}", target_consonant, target.virama);
        map.insert(source_consonant.clone(), t);
    }
    for (source_punctuation, target_punctuation) in
        source.punctuation.iter().zip(target.punctuation)
    {
        map.insert(source_punctuation.into(), target_punctuation.into());
    }
    for (source_digit, target_digit) in source.digits.iter().zip(target.digits) {
        map.insert(source_digit.into(), target_digit.into());
    }
    map.into_iter()
        .map(|(k, v)| (normalize(&k), normalize(&v)))
        .collect()
}

fn map_alphabets(source: ParsedScript, target: ParsedScript) -> HashMap<String, String> {
    assert!(source.style == Style::Alphabet && target.style == Style::Alphabet);
    let mut map = HashMap::new();
    for (source_vowel, target_vowel) in source.vowels.iter().zip(target.vowels) {
        map.insert(source_vowel.into(), target_vowel.into());
    }
    for (source_consonant, target_consonant) in source.consonants.iter().zip(target.consonants) {
        map.insert(source_consonant, target_consonant);
    }
    for (source_punctuation, target_punctuation) in
        source.punctuation.iter().zip(target.punctuation)
    {
        map.insert(source_punctuation.into(), target_punctuation.into());
    }
    for (source_digit, target_digit) in source.digits.iter().zip(target.digits) {
        map.insert(source_digit.into(), target_digit.into());
    }
    map.into_iter()
        .map(|(k, v)| (normalize(&k), normalize(&v)))
        .collect()
}

fn normalize(s: &str) -> String {
    let s: String = s.nfd().collect();
    let s = s.replace("◌", "");
    s.nfc().collect()
}
