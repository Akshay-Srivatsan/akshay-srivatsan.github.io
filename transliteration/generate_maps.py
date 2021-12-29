#!/usr/bin/env python
import json
import unicodedata

from scripts import *


def mapping_alphabet_abugida(start, end):
    mapping = {}
    for vowel in start["vowels"]:
        s = start["vowels"][vowel]
        e = end["vowels"][vowel]
        if len(e) == 2:
            e = e[0]
        if s not in mapping:
            mapping[s] = e
        for modifier in start["vowel_modifiers"]:
            sm = start["vowel_modifiers"][modifier]
            em = end["vowel_modifiers"][modifier]
            if (s + sm) not in mapping:
                mapping[s + sm] = e + em
    silencer = end["silencer"]
    for consonant in start["consonants"]:
        s = start["consonants"][consonant]
        e = end["consonants"][consonant]
        if s not in mapping:
            mapping[s] = e + silencer
        if s + "a" not in mapping:
            mapping[s + "a"] = e
        for vowel in start["vowels"]:
            sv = start["vowels"][vowel]
            ev = end["vowels"][vowel]
            if len(ev) == 2:
                ev = ev[1]
            if (s + sv) not in mapping:
                mapping[s + sv] = e + ev
            for modifier in start["vowel_modifiers"]:
                sm = start["vowel_modifiers"][modifier]
                em = end["vowel_modifiers"][modifier]
                if (s + sv + sm) not in mapping:
                    mapping[s + sv + sm] = e + ev + em
    for geminate in start["geminates"]:
        s = start["geminates"][geminate]
        if geminate in end["geminates"]:
            e = end["geminates"][geminate]
        else:
            e = end["consonants"][geminate] + silencer + end["consonants"][geminate]
        if s not in mapping:
            mapping[s] = e + silencer
        if s + "a" not in mapping:
            mapping[s + "a"] = e
        for vowel in start["vowels"]:
            sv = start["vowels"][vowel]
            ev = end["vowels"][vowel]
            if len(ev) == 2:
                ev = ev[1]
            if (s + sv) not in mapping:
                mapping[s + sv] = e + ev
            for modifier in start["vowel_modifiers"]:
                sm = start["vowel_modifiers"][modifier]
                em = end["vowel_modifiers"][modifier]
                if (s + sv + sm) not in mapping:
                    mapping[s + sv + sm] = e + ev + em
    for geminate in end["geminates"]:
        e = end["geminates"][geminate]
        if geminate in start["geminates"]:
            s = start["geminates"][geminate]
        else:
            s = start["consonants"][geminate] + start["consonants"][geminate]
        if s not in mapping:
            mapping[s] = e + silencer
        if s + "a" not in mapping:
            mapping[s + "a"] = e
        for vowel in start["vowels"]:
            sv = start["vowels"][vowel]
            ev = end["vowels"][vowel]
            if len(ev) == 2:
                ev = ev[1]
            if (s + sv) not in mapping:
                mapping[s + sv] = e + ev
            for modifier in start["vowel_modifiers"]:
                sm = start["vowel_modifiers"][modifier]
                em = end["vowel_modifiers"][modifier]
                if (s + sv + sm) not in mapping:
                    mapping[s + sv + sm] = e + ev + em
    for numeral in range(0, 10):
        s = start["numerals"][numeral]
        e = end["numerals"][numeral]
        if s not in mapping:
            mapping[s] = e
    for punctuation in start["punctuation"]:
        s = start["punctuation"][punctuation]
        e = end["punctuation"][punctuation]
        if s not in mapping:
            mapping[s] = e
    return mapping


def mapping_abugida_alphabet(start, end):
    mapping = {}
    for vowel in start["vowels"]:
        s = start["vowels"][vowel]
        e = end["vowels"][vowel]
        if len(s) == 2:
            s = s[0]
        if s not in mapping:
            mapping[s] = e
        for modifier in start["vowel_modifiers"]:
            sm = start["vowel_modifiers"][modifier]
            em = end["vowel_modifiers"][modifier]
            if (s + sm) not in mapping:
                mapping[s + sm] = e + em
    silencer = start["silencer"]
    for consonant in start["consonants"]:
        s = start["consonants"][consonant]
        e = end["consonants"][consonant]
        if s not in mapping:
            mapping[s] = e + "a"
        if s + silencer not in mapping:
            mapping[s + silencer] = e
        for vowel in start["vowels"]:
            sv = start["vowels"][vowel]
            ev = end["vowels"][vowel]
            if len(sv) == 2:
                sv = sv[1]
            if (s + sv) not in mapping:
                mapping[s + sv] = e + ev
            for modifier in start["vowel_modifiers"]:
                sm = start["vowel_modifiers"][modifier]
                em = end["vowel_modifiers"][modifier]
                if (s + sv + sm) not in mapping:
                    mapping[s + sv + sm] = e + ev + em
    for geminate in start["geminates"]:
        s = start["geminates"][geminate]
        if geminate in end["geminates"]:
            e = end["geminates"][geminate]
        else:
            e = end["consonants"][geminate] + end["consonants"][geminate]
        if s not in mapping:
            mapping[s] = e + "a"
        if s + silencer not in mapping:
            mapping[s + silencer] = e
        for vowel in start["vowels"]:
            sv = start["vowels"][vowel]
            ev = end["vowels"][vowel]
            if len(sv) == 2:
                sv = sv[1]
            if (s + sv) not in mapping:
                mapping[s + sv] = e + ev
            for modifier in start["vowel_modifiers"]:
                sm = start["vowel_modifiers"][modifier]
                em = end["vowel_modifiers"][modifier]
                if (s + sv + sm) not in mapping:
                    mapping[s + sv + sm] = e + ev + em
    for geminate in end["geminates"]:
        if geminate in start["geminates"]:
            continue
        e = end["geminates"][geminate]
        if geminate not in start["consonants"]:
            continue
        s = (
            start["consonants"][geminate]
            + start["silencer"]
            + start["consonants"][geminate]
        )
        if s not in mapping:
            mapping[s] = e + "a"
        if s + silencer not in mapping:
            mapping[s + silencer] = e
        for vowel in start["vowels"]:
            sv = start["vowels"][vowel]
            ev = end["vowels"][vowel]
            if len(sv) == 2:
                sv = sv[1]
            if (s + sv) not in mapping:
                mapping[s + sv] = e + ev
            for modifier in start["vowel_modifiers"]:
                sm = start["vowel_modifiers"][modifier]
                em = end["vowel_modifiers"][modifier]
                if (s + sv + sm) not in mapping:
                    mapping[s + sv + sm] = e + ev + em
    for numeral in range(0, 10):
        s = start["numerals"][numeral]
        e = end["numerals"][numeral]
        if s not in mapping:
            mapping[s] = e
    for punctuation in start["punctuation"]:
        s = start["punctuation"][punctuation]
        e = end["punctuation"][punctuation]
        if s not in mapping:
            mapping[s] = e
    return mapping


def mapping_same(start, end):
    mapping = {}
    t = start["type"]
    for v in start["vowels"]:
        if t == "abugida":
            if start["vowels"][v][0] not in mapping:
                mapping[start["vowels"][v][0]] = end["vowels"][v][0]
            if start["vowels"][v][1] not in mapping:
                mapping[start["vowels"][v][1]] = end["vowels"][v][1]
        else:
            if start["vowels"][v] not in mapping:
                mapping[start["vowels"][v]] = end["vowels"][v]
    for m in start["vowel_modifiers"]:
        if start["vowel_modifiers"][m] not in mapping:
            mapping[start["vowel_modifiers"][m]] = end["vowel_modifiers"][m]
    for c in start["consonants"]:
        if start["consonants"][c] not in mapping:
            mapping[start["consonants"][c]] = end["consonants"][c]
    for g in start["geminates"]:
        if start["geminates"][g] not in mapping:
            if g in end["geminates"]:
                mapping[start["geminates"][g]] = end["geminates"][g]
            else:
                if t == "abugida":
                    mapping[start["geminates"][g]] = (
                        end["consonants"][g] + end["silencer"] + end["consonants"][g]
                    )
                else:
                    mapping[start["geminates"][g]] = (
                        end["consonants"][g] + end["consonants"][g]
                    )
    for g in end["geminates"]:
        if g in start["geminates"]:
            continue
        if t == "abugida":
            if (
                start["consonants"][g] + start["silencer"] + start["consonants"][g]
                not in mapping
            ):
                mapping[
                    start["consonants"][g] + start["silencer"] + start["consonants"][g]
                ] = end["geminates"][g]
        else:
            if start["consonants"][g] + start["consonants"][g] not in mapping:
                mapping[start["consonants"][g] + start["consonants"][g]] = end[
                    "geminates"
                ][g]
    for i in range(0, 10):
        mapping[start["numerals"][i]] = end["numerals"][i]
    for p in start["punctuation"]:
        if start["punctuation"][p] not in mapping:
            mapping[start["punctuation"][p]] = end["punctuation"][p]
    mapping[start["silencer"]] = end["silencer"]
    return mapping


def fix_str(s, m):
    if m != TAMIL_GRANTHA:
        s = unicodedata.normalize("NFD", s)
    s = s.replace("◌", "")
    s = s.replace("ː̃", "◌̃ː")
    s = s.replace("◌", "")
    if m == TAMIL_EXT:
        for vowel in m["vowels"]:
            _, v = m["vowels"][vowel]
            v = fix_str(v, None)
            s = s.replace("²" + v, v + "²")
            s = s.replace("³" + v, v + "³")
            s = s.replace("⁴" + v, v + "⁴")
        v = fix_str(m["silencer"], None)
        s = s.replace("²" + v, v + "²")
        s = s.replace("³" + v, v + "³")
        s = s.replace("⁴" + v, v + "⁴")
    s = s.replace("◌", "")
    if m == TAMIL_GRANTHA:
        return s
    return unicodedata.normalize("NFC", s)


def preprocess_mapping(m):
    if m["geminizer"] is not None:
        for c in m["consonants"]:
            if c not in m["geminates"]:
                m["geminates"][c] = m["consonants"][c] + m["geminizer"]
    return m


def generate_mapping(start, end):
    start = preprocess_mapping(start)
    end = preprocess_mapping(end)
    # print(f"Generating mapping from {start['name']} to {end['name']}.")
    mapping = {}
    if start["type"] == "alphabet" and end["type"] == "abugida":
        mapping = mapping_alphabet_abugida(start, end)
    if start["type"] == "abugida" and end["type"] == "alphabet":
        mapping = mapping_abugida_alphabet(start, end)
    if start["type"] == end["type"]:
        mapping = mapping_same(start, end)

    mapping = {k: mapping[k] for k in mapping if mapping[k] is not None}
    mapping = {fix_str(k, start): fix_str(mapping[k], end) for k in mapping}
    return mapping


with open("tamil.js", "w") as tamil:
    data = {
        "to_iso": generate_mapping(TAMIL, ISO),
        "to_ipa": generate_mapping(TAMIL, TAMIL_IPA),
        "to_devanagari": generate_mapping(TAMIL, DEVANAGARI),
        "to_grantha": generate_mapping(TAMIL, GRANTHA),
        "to_brahmi": generate_mapping(TAMIL, TAMIL_BRAHMI),
    }
    tamil.write(
        "let mapping = " + json.dumps(data, indent=4) + ";",
    )

with open("sanskrit.js", "w") as sanskrit:
    data = {
        "to_devanagari": generate_mapping(ISO, DEVANAGARI),
        "to_ipa": generate_mapping(ISO, SANSKRIT_IPA),
        "to_tamil": generate_mapping(ISO, TAMIL_EXT),
        "to_grantha": generate_mapping(ISO, GRANTHA),
        "to_tamil_grantha": generate_mapping(ISO, TAMIL_GRANTHA),
        "to_brahmi": generate_mapping(ISO, BRAHMI),
    }
    sanskrit.write(
        "let mapping = " + json.dumps(data, indent=4) + ";",
    )
