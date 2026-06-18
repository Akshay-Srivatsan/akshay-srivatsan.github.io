# Authoring Notation

Tamil, Hindi, and Sanskrit pages are authored in Latin source scripts and rendered
to their display scripts by the Hakyll build.

The notation is lowercase and does not use case or punctuation for phonetic
contrast. Long vowels and retroflex/sibilant distinctions use ordinary
precomposed diacritics such as `ā`, `ī`, `ū`, `ṭ`, `ḍ`, `ṇ`, `ś`, and `ṣ`.

## Tamil

Tamil source uses a Tanglish-oriented Latin notation that normalizes to
phonemic Latin Tamil before rendering:

- Vowels: `a ā i ī u ū e ē ai o ō au`
- Plain `n` is inferred as dental `n` word-initially and before `t`, and as
  alveolar `ṉ` elsewhere. Use `nh` to force dental `n` in an exceptional
  position, or explicit `ṉ` to force alveolar `ṉ`.
- Common Tanglish spellings are accepted: `g` for written `k`, `ng` for `ṅ`,
  `ḍ` for written `ṭ`, `sh` for `ṣ`, `zh` for `ḻ`, `rh` for `ṟ`, `rr` for
  `ṟṟ`, and `ksh` for `kṣ`.
- The common surface cluster `ndr` normalizes to written `ṉṟ`, so `nandri`,
  `mūndru`, and `endrāl` render as `naṉṟi`, `mūṉṟu`, and `eṉṟāl`.
- Plain `n` assimilates before following stops after voiced authoring stops are
  normalized: `nk/ng -> ṅk`, `nc/nj -> ñc/ñj`, `nṭ/nḍ -> ṇṭ`, `nt -> nt`,
  and `np -> mp`.
- Suffixal written `-kaḷ` may be authored as pronounced `-gaḷ`, so forms like
  `vakuppugaḷ` and `naṭanangaḷ` render as `vakuppukaḷ` and `naṭaṉaṅkaḷ`.
- Retroflexes still use underdots where they are contrastive: `ṭ`, `ṇ`, and
  `ḷ`.
- The canonical phonemic tokens still work as overrides: `k c ṭ t p ṟ ṅ ñ ṇ n m
  ṉ y r l v ḻ ḷ ś ṣ s h j kṣ f z`.

## Hindi

Hindi source is pronunciation-oriented Hinglish/IAST:

- Vowels: `a ā i ī u ū e ai o au`; rare vocalic vowels use `r̥`, `r̥̄`,
  `l̥`, and `l̥̄`.
- Consonants follow IAST-style spelling, with `ṛ` and `ṛh` for ड़ and ढ़.
- Nasalized vowels are written as readers usually perceive them: `main`, `men`,
  `hūn`, `hain`, `āngrezī`, `kakśāon`, and similar forms. The build normalizes
  these to `maiṁ`, `meṁ`, `hūm̐`, `haiṁ`, etc. before rendering Devanagari or
  ISO.
- Devanagari output inserts orthographic inherent vowels from the pronounced
  source. Use explicit `a` where the spelling needs a vowel that the rule cannot
  infer.

## Sanskrit

Sanskrit source uses underlying forms before external sandhi:

- `sh` is accepted for `ṣ`, so `bhāshā` renders as `bhāṣā`.
- Write underlying final `s`, not display `ḥ`: `chātras asmi` renders as
  `chātro ’smi`.
- Write vowel-final stems before sandhi: `rāṣṭrapati asmi` renders with
  `iko yaṇ aci`.
- The implementation encodes the Sanskrit boundary rules as an ordered list of
  named Paninian-style sūtras in `Site.Transliterate`.
