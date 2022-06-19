function setup(container, target, config) {
    let names = config.map((x) => x[0]);
    let codes = config.map((x) => x[1]);
    let isos = config.map((x) => x[2]);
    let mappings = config.map((x) => x[3]);

    // Create options in select
    let options = config.map(([name, code, iso, mapping]) => {
        let option = document.createElement('option');
        option.setAttribute('value', code);
        option.innerText = name;
        return option;
    });
    options.map((o) => target.appendChild(o));

    // Make the selector visible
    container.style.display = 'block';

    function set_script(i) {
        let code = codes[i];
        let iso = isos[i];
        let mapping = mappings[i];

        // If this was set somehow else, set the selector to the right index.
        target.selectedIndex = i;

        // Hide redundant name versions but show non-redundant ones.
        for (let j = 0; j < codes.length; j++) {
            let element = document.getElementById(`${codes[j]}-name`);
            if (element) {
                element.style.display = i === j ? 'none' : 'inline';
            }
        }

        reset();
        let query = '';
        if (mapping !== null) {
            // Transcribe and update path.
            transcribe_document(mapping);
            query = `?${code}`;
        }
        // This isn't a new page, so replace the history entry silently.
        window.history.replaceState(
            '',
            document.title,
            window.location.pathname + query
        );
        document.documentElement.lang = iso;
    }

    // When the selection changes, switch to the right script.
    target.addEventListener('change', ({ target: { selectedIndex } }) => {
        set_script(selectedIndex);
    });

    // Check if there is a query already, and if so dispatch it correctly.
    if (window.location.search) {
        let index = codes.indexOf(window.location.search.slice(1));
        if (index !== -1) {
            set_script(index);
        }
    }
}
