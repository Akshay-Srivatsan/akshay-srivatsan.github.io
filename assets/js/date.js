let months_accusative = [
    '',
    'Jānuāriās',
    'Februāriās',
    'Mārtiās',
    'Aprīlēs',
    'Maiās',
    'Jūniās',
    'Jūlia',
    'Augustās',
    'Septembrēs',
    'Octōbrēs',
    'Novembrēs',
    'Decembrēs',
    'Jānuāriās',
];
let months_ablative = [
    '',
    'Jānuāriīs',
    'Februāriīs',
    'Mārtiīs',
    'Aprīlēs',
    'Maiīs',
    'Jūniīs',
    'Jūlia',
    'Augustīs',
    'Septembrēs',
    'Octōbrēs',
    'Novembrēs',
    'Decembrēs',
    'Jānuāriīs',
];

let numbers_accusative = [
    'nihil',
    'prīmum',
    'secundum',
    'tertium',
    'quārtum',
    'quīntum',
    'sextum',
    'septimum',
    'octāvum',
    'nōnum',
    'decimum',
    'ūndecimum',
    'duodecimum',
    'tertium decimum',
    'quārtum decimum',
    'quīntum decimum',
    'sextum decimum',
    'septimum decimum',
    'duodēvicēsimus',
    'ūndēvīcēsimus',
    'vīcēsimus',
];

let months_short = [
    '',
    'Iān.',
    'Feb.',
    'Mār.',
    'Apr.',
    'Mai.',
    'Iūn.',
    'Iūl.',
    'Aug.',
    'Sept.',
    'Oct.',
    'Nov.',
    'Dec.',
    'Iān.',
];

let numbers_short = [
    'N',
    'I',
    'II',
    'III',
    'IV',
    'V',
    'VI',
    'VII',
    'VIII',
    'IX',
    'X',
    'XI',
    'XII',
    'XIII',
    'XIV',
    'XV',
    'XVI',
    'XVII',
    'XVIII',
    'XIX',
    'XX',
];

let date_label = 'hodie est';
let ante_diem = 'ante diem';
let nonas = 'nōnās';
let nonis = 'nōnīs';
let nonas_singular = 'nōnas';
let idus = 'īdūs';
let idibus = 'īdibus';
let kalendis = 'kalendīs';
let kalendas = 'kalendās';
let kalendas_capitalized = 'Kalendas';
let pridie = 'prīdiē';
let kal_short = 'kal.';
let ante_diem_short = 'a.d.';
let non_short = 'nōn.';
let id_short = 'eīd.';
let pridie_short = 'prīd.';

let dateScript = document.currentScript;

function dataList(name, fallback) {
    if (!dateScript || !(name in dateScript.dataset)) {
        return fallback;
    }
    return dateScript.dataset[name].split('\t');
}

function dataText(name, fallback) {
    if (!dateScript || !(name in dateScript.dataset)) {
        return fallback;
    }
    return dateScript.dataset[name];
}

months_accusative = dataList('monthsAccusative', months_accusative);
months_ablative = dataList('monthsAblative', months_ablative);
numbers_accusative = dataList('numbersAccusative', numbers_accusative);
months_short = dataList('monthsShort', months_short);
numbers_short = dataList('numbersShort', numbers_short);
date_label = dataText('dateLabel', date_label);
ante_diem = dataText('anteDiem', ante_diem);
nonas = dataText('nonas', nonas);
nonis = dataText('nonis', nonis);
nonas_singular = dataText('nonasSingular', nonas_singular);
idus = dataText('idus', idus);
idibus = dataText('idibus', idibus);
kalendis = dataText('kalendis', kalendis);
kalendas = dataText('kalendas', kalendas);
kalendas_capitalized = dataText('kalendasCapitalized', kalendas_capitalized);
pridie = dataText('pridie', pridie);
kal_short = dataText('kalShort', kal_short);
ante_diem_short = dataText('anteDiemShort', ante_diem_short);
non_short = dataText('nonShort', non_short);
id_short = dataText('idShort', id_short);
pridie_short = dataText('pridieShort', pridie_short);

function roman_date(date) {
    let day = date.getDate();
    let month = date.getMonth() + 1;
    let year = date.getYear();

    let last_day_of_month = new Date(year, month, 0).getDate();

    let kalends = 1;
    let nones = 5;
    let ides = 13;
    if ([3, 5, 7, 10].indexOf(month) != -1) {
        nones = 7;
        ides = 15;
    }

    if (day == kalends) {
        return `${kalendis} ${months_ablative[month]}`;
    }

    if (day > kalends && day < nones - 1) {
        return `${ante_diem} ${numbers_accusative[nones - day + 1]} ${nonas} ${
            months_accusative[month]
        }`;
    }

    if (day == nones - 1) {
        return `${pridie} ${nonas_singular} ${months_accusative[month]}`;
    }

    if (day == nones) {
        return `${nonis} ${months_ablative[month]}`;
    }

    if (day > nones && day < ides - 1) {
        return `${ante_diem} ${numbers_accusative[ides - day + 1]} ${idus} ${
            months_accusative[month]
        }`;
    }

    if (day == ides - 1) {
        return `${pridie} ${idus} ${months_accusative[month]}`;
    }

    if (day == ides) {
        return `${idibus} ${months_ablative[month]}`;
    }

    if (day > ides && day < last_day_of_month) {
        return `${ante_diem} ${
            numbers_accusative[last_day_of_month + 1 - day + 1]
        } ${kalendas_capitalized} ${months_accusative[month + 1]}`;
    }

    if (day == last_day_of_month) {
        return `${pridie} ${kalendas} ${months_accusative[month + 1]}`;
    }
}

function shortRomanDate(date) {
    let day = date.getDate();
    let month = date.getMonth() + 1;
    let year = date.getYear();

    let lastDayOfMonth = new Date(year, month, 0).getDate();

    let kalends = 1;
    let nones = 5;
    let ides = 13;
    if ([3, 5, 7, 10].indexOf(month) != -1) {
        nones = 7;
        ides = 15;
    }

    if (day == kalends) {
        return `${kal_short} ${months_short[month]}`;
    }

    if (day > kalends && day < nones - 1) {
        return (
            ante_diem_short +
            ' ' +
            numbers_short[nones - day + 1] +
            ' ' +
            non_short +
            ' ' +
            months_short[month]
        );
    }

    if (day == nones - 1) {
        return `${pridie_short} ${non_short} ${months_short[month]}`;
    }

    if (day == nones) {
        return `${non_short} ${months_short[month]}`;
    }

    if (day > nones && day < ides - 1) {
        return (
            ante_diem_short +
            ' ' +
            numbers_short[ides - day + 1] +
            ' ' +
            id_short +
            ' ' +
            months_short[month]
        );
    }

    if (day == ides - 1) {
        return `${pridie_short} ${id_short} ${months_short[month]}`;
    }

    if (day == ides) {
        return `${id_short} ${months_short[month]}`;
    }

    if (day > ides && day < lastDayOfMonth) {
        return (
            ante_diem_short +
            ' ' +
            numbers_short[lastDayOfMonth + 1 - day + 1] +
            ' ' +
            kal_short +
            ' ' +
            months_short[month + 1]
        );
    }

    if (day == lastDayOfMonth) {
        return `${pridie_short} ${kal_short} ${months_short[month + 1]}`;
    }
}

function hodie() {
    return roman_date(new Date());
}

function hodieBreve() {
    return shortRomanDate(new Date());
}

document.addEventListener('DOMContentLoaded', () => {
    let date = document.getElementById('date');
    if (date) {
        date.innerHTML = `${date_label}: ${hodie()} (${hodieBreve()})`;
    }
});
