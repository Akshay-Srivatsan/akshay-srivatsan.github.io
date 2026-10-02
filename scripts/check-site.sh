#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
HUGO=${HUGO:-hugo}
XMLLINT=${XMLLINT:-xmllint}
task_tmp=$(mktemp -d "${TMPDIR:-/tmp}/site-check.XXXXXX")
trap 'rm -rf "$task_tmp"' EXIT HUP INT TERM

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

equal() {
    [ "$1" = "$2" ] || fail "$3: expected [$2], got [$1]"
}

html() {
    "$XMLLINT" --html --xpath "$2" "$1" 2>/dev/null
}

xml() {
    "$XMLLINT" --xpath "$2" "$1"
}

decode_path() {
    # Decode URI bytes, including UTF-8 filenames, without a scripting runtime.
    LC_ALL=C awk '
        function hex(c) { return index("0123456789abcdef", tolower(c)) - 1 }
        {
            for (i=1; i<=length($0); i++) {
                c=substr($0,i,1); pair=substr($0,i+1,2)
                if (c=="%" && pair ~ /^[0-9a-fA-F][0-9a-fA-F]$/) {
                    printf "%c", hex(substr(pair,1,1))*16 + hex(substr(pair,2,1)); i+=2
                } else { printf "%s", c }
            }
            print ""
        }'
}

check_links() {
    find "$1" -name '*.html' -type f | while IFS= read -r page; do
        attributes=$(html "$page" '//a/@href | //img/@src | //link/@href')
        printf '%s\n' "$attributes" | awk -F '"' '{ for (i=2; i<=NF; i+=2) print $i }' |
        while IFS= read -r link; do
            link_path=${link%%\#*}
            link_path=${link_path%%\?*}
            case "$link_path" in
                https://aks.io/*) link_path=${link_path#https://aks.io} ;;
                http://aks.io/*) link_path=${link_path#http://aks.io} ;;
                //*) continue ;;
                *:*) continue ;;
            esac
            link_path=$(printf '%s\n' "$link_path" | decode_path)
            case "$link_path" in
                '') target=$page ;;
                /*) target="$1$link_path" ;;
                *) target="${page%/*}/$link_path" ;;
            esac
            if [ -d "$target" ]; then
                [ -f "${target%/}/index.html" ] || fail "$page: missing index for $link"
            else
                [ -f "$target" ] || fail "$page: broken link $link"
            fi
        done
    done
}

# Check the real build, including every copied file and every local HTML link.
[ -f public/index.html ] || fail 'run make build before checking'
find static -type f | while IFS= read -r source; do
    cmp "$source" "public/${source#static/}" || fail "static file changed: $source"
done
check_links public
for language in en la sa ta; do
    prefix="/$language"
    [ "$language" != en ] || prefix=''
    page="public$prefix/index.html"
    equal "$(html "$page" 'string(/html/@lang)')" "$language" 'homepage language'
    equal "$(html "$page" 'count(//main//span[contains(@class,"ipa")])')" 1 'pronunciation preserved'
    equal "$(html "$page" 'count(//main//*[@id="tamil-name" or @id="iso-name" or @id="grantha-name"])')" 0 'alternate names removed'
    [ ! -e "public/$language.html" ] || fail 'old language URL survived'
    if [ -f "content/courses.$language.md" ]; then
        [ -f "public$prefix/courses/index.html" ] || fail 'authored content page missing'
    else
        [ ! -e "public$prefix/courses/index.html" ] || fail 'unwritten page translation generated'
    fi
    "$XMLLINT" --noout "public$prefix/blog/index.xml"
done
equal "$(html public/ta/index.html 'count(//main//div[@lang="en"]/dl)')" 1 'Tamil research block preserves English'
equal "$(html public/la/index.html 'count(//main//div[@lang="en"]/dl)')" 1 'Latin research block preserves English'
equal "$(html public/jobs/index.html 'count(//main//table)')" 6 'tables preserved'
equal "$(html public/shows/index.html 'count(//main//sup/a)')" 4 'footnotes preserved'

# Fixture content is isolated from published posts, so future posts cannot affect tests.
mkdir -p "$task_tmp/content/blog/archive" "$task_tmp/content/tags"
cp content/_index.*.md "$task_tmp/content/"
cp content/blog/_index.*.md "$task_tmp/content/blog/"
cp content/blog/archive/_index.*.md "$task_tmp/content/blog/archive/"
cp content/tags/_index.*.md "$task_tmp/content/tags/"
cp -R data "$task_tmp/data"
printf '\nfixture-query: https://example.test/?a=1&b=two\n' >> "$task_tmp/data/links.yaml"

build_fixture() {
    HUGO_DATADIR="$task_tmp/data" "$HUGO" --contentDir "$task_tmp/content" --destination "$task_tmp/public" \
        --cacheDir "$task_tmp/cache" \
        --cleanDestinationDir --printI18nWarnings --panicOnWarning > "$task_tmp/build.log" 2>&1
}

build_fixture || { cat "$task_tmp/build.log"; fail 'empty fixture build'; }
equal "$(html "$task_tmp/public/blog/index.html" 'string(//main/p)')" 'No posts yet.' 'empty blog'
equal "$(xml "$task_tmp/public/blog/index.xml" 'count(/rss/channel/item)')" 0 'empty feed'
check_links "$task_tmp/public"

# Exercise all 15 nonempty subsets of the four supported languages.
mask=1
while [ "$mask" -le 15 ]; do
    mkdir -p "$task_tmp/content/blog/subset-$mask"
    cp static/assets/img/favicon.png "$task_tmp/content/blog/subset-$mask/photo.png"
    case "$mask" in
        [1-7]) date=2024-12-01 ;;
        15) date=2025-02-03 ;;
        *) date=2025-01-02 ;;
    esac
    bit=1
    for language in en la sa ta; do
        if [ "$((mask & bit))" -ne 0 ]; then
            tag_list="$language-tag"
            [ "$language" != ta ] || tag_list="$tag_list, தமிழ்"
            cat > "$task_tmp/content/blog/subset-$mask/index.$language.md" <<EOF
---
title: '$language post $mask & <test> "quoted"'
date: $date
tags: [$tag_list]
summary: 'Summary & **bold** text.'
---

Markdown **body**, தமிழ், संस्कृतम्, and <span lang="en">English</span>.

[A local image](/assets/img/portrait_2026.jpg).

![Bundle image](photo.png)

[Shared GitHub]({{< link "github" >}}) and [Query]({{< link "fixture-query" >}}).
EOF
        fi
        bit=$((bit * 2))
    done
    mask=$((mask + 1))
done

mkdir -p "$task_tmp/content/blog/draft" "$task_tmp/content/blog/future"
cat > "$task_tmp/content/blog/draft/index.en.md" <<'EOF'
---
title: Draft
date: 2025-01-01
draft: true
tags: [unpublished]
---
Hidden draft.
EOF
cat > "$task_tmp/content/blog/future/index.ta.md" <<'EOF'
---
title: Future
date: 2999-01-01
tags: [unpublished]
---
Hidden future post.
EOF
build_fixture || { cat "$task_tmp/build.log"; fail 'multilingual fixture build'; }
check_links "$task_tmp/public"

current_bit=1
for language in en la sa ta; do
    prefix="/$language"
    [ "$language" != en ] || prefix=''
    listing="$task_tmp/public$prefix/blog/index.html"
    archive="$task_tmp/public$prefix/blog/archive/index.html"
    feed="$task_tmp/public$prefix/blog/index.xml"
    equal "$(html "$listing" 'count(//main//a[@class="post-title"])')" 15 'one listing entry per post'
    equal "$(html "$archive" 'count(//main//a[@class="post-title"])')" 15 'one archive entry per post'
    equal "$(html "$archive" 'count(//main/h2)')" 2 'archive years'
    equal "$(html "$archive" 'count(//main/h3)')" 3 'archive months'
    equal "$(html "$listing" 'string((//main//a[@class="post-title"])[1]/@href)')" "$prefix/blog/subset-15/" 'newest post first'
    equal "$(xml "$feed" 'count(/rss/channel/item)')" 15 'one RSS item per post'
    equal "$(xml "$feed" 'string(/rss/channel/language)')" "$language" 'feed language'
    equal "$(xml "$feed" 'contains(string(/rss/channel/item[1]/description), "Summary &")')" true 'RSS summary escaping'
    equal "$(html "$task_tmp/public$prefix/tags/$language-tag/index.html" 'count(//main//a[@class="post-title"])')" 8 'language-specific tags'
    [ ! -e "$task_tmp/public$prefix/tags/unpublished" ] || fail 'unpublished tag leaked'
    [ ! -e "$task_tmp/public$prefix/blog/draft" ] || fail 'draft leaked'
    [ ! -e "$task_tmp/public$prefix/blog/future" ] || fail 'future post leaked'
    mask=1
    while [ "$mask" -le 15 ]; do
        selected=''
        available=0
        bit=1
        for candidate in en la sa ta; do
            if [ "$((mask & bit))" -ne 0 ]; then
                available=$((available + 1))
                [ -n "$selected" ] || selected=$candidate
            fi
            bit=$((bit * 2))
        done
        if [ "$((mask & current_bit))" -ne 0 ]; then
            selected=$language
            [ -f "$task_tmp/public$prefix/blog/subset-$mask/index.html" ] || fail 'authored translation missing'
            expected_tags=1
            [ "$language" != ta ] || expected_tags=2
            equal "$(html "$task_tmp/public$prefix/blog/subset-$mask/index.html" 'count(//article/header/p[@class="post-tags"]/a)')" "$expected_tags" 'post tag links'
            equal "$(html "$task_tmp/public$prefix/blog/subset-$mask/index.html" "string(//article//a[.='Query']/@href)")" 'https://example.test/?a=1&b=two' 'shared URL escaping'
            equal "$(html "$task_tmp/public$prefix/blog/subset-$mask/index.html" "count(//article//img[@alt='Bundle image'])")" 1 'bundle image rendered'
        else
            [ ! -e "$task_tmp/public$prefix/blog/subset-$mask" ] || fail 'unwritten translation generated'
        fi
        selected_prefix="/$selected"
        [ "$selected" != en ] || selected_prefix=''
        target="$selected_prefix/blog/subset-$mask/"
        title="$selected post $mask & <test> \"quoted\""
        equal "$(html "$listing" "string(//main//a[@class='post-title' and @href='$target'])")" "$title" 'preferred translation title'
        equal "$(html "$listing" "count(//li[a[@href='$target']]//span[@class='post-languages']/a)")" "$available" 'available languages'
        equal "$(xml "$feed" "string(/rss/channel/item[link='https://aks.io$target']/title)")" "$title" 'RSS matches listing'
        guid_query="string(/rss/channel/item[contains(link, '/blog/subset-$mask/')]/guid)"
        equal "$(xml "$feed" "$guid_query")" "$(xml "$task_tmp/public/blog/index.xml" "$guid_query")" 'GUID independent of selected language'
        mask=$((mask + 1))
    done
    current_bit=$((current_bit * 2))
done
equal "$(html "$task_tmp/public/ta/tags/தமிழ்/index.html" 'count(//main//a[@class="post-title"])')" 8 'native-script tag page'

# The native new-content workflow creates an unpublished starter post.
"$HUGO" new content --contentDir "$task_tmp/content" --kind blog blog/new-post/index.en.md > "$task_tmp/new.log" 2>&1
case "$(cat "$task_tmp/content/blog/new-post/index.en.md")" in
    *'draft: true'*) ;;
    *) fail 'new post was not a draft' ;;
esac
build_fixture || { cat "$task_tmp/build.log"; fail 'starter draft build'; }
[ ! -e "$task_tmp/public/blog/new-post" ] || fail 'starter draft published'

# Shared link keys are validated by Hugo, without a separate database generator.
cat > "$task_tmp/content/bad-link.en.md" <<'EOF'
---
title: Broken link fixture
---
[Broken]({{< link "does-not-exist" >}}).
EOF
if build_fixture; then fail 'unknown shared link key accepted'; fi
case "$(cat "$task_tmp/build.log")" in
    *'unknown link key'*'does-not-exist'*) ;;
    *) cat "$task_tmp/build.log"; fail 'missing link failed for the wrong reason' ;;
esac
mv "$task_tmp/content/bad-link.en.md" "$task_tmp/bad-link.en.md"

# A newly added translation changes the selected link without changing the RSS GUID.
guid_query="string(/rss/channel/item[contains(link, '/blog/subset-8/')]/guid)"
original_guid=$(xml "$task_tmp/public/blog/index.xml" "$guid_query")
cp "$task_tmp/content/blog/subset-8/index.ta.md" "$task_tmp/content/blog/subset-8/index.en.md"
build_fixture || { cat "$task_tmp/build.log"; fail 'added translation build'; }
equal "$(xml "$task_tmp/public/blog/index.xml" "$guid_query")" "$original_guid" 'GUID survives added translation'
equal "$(xml "$task_tmp/public/blog/index.xml" "count(/rss/channel/item[link='https://aks.io/blog/subset-8/'])")" 1 'new translation selected'

# Independent draft translations disappear from switchers and do not mask published variants.
sed 's/^tags:/draft: true\ntags:/' "$task_tmp/content/blog/subset-15/index.la.md" > "$task_tmp/draft.md"
mv "$task_tmp/draft.md" "$task_tmp/content/blog/subset-15/index.la.md"
build_fixture || { cat "$task_tmp/build.log"; fail 'draft translation build'; }
[ ! -e "$task_tmp/public/la/blog/subset-15" ] || fail 'draft translation rendered'
equal "$(html "$task_tmp/public/blog/subset-15/index.html" 'count(//main//nav/a[@hreflang="la"])')" 0 'draft translation hidden from switcher'
equal "$(html "$task_tmp/public/la/blog/index.html" "count(//a[@class='post-title' and @href='/blog/subset-15/'])")" 1 'published fallback for draft translation'

mkdir -p "$task_tmp/content/blog/invalid"
cat > "$task_tmp/content/blog/invalid/index.en.md" <<'EOF'
---
title: Invalid
date: 2025-01-01
---
Invalid fixture.
EOF
sed 's/2025-01-01/2025-01-02/' "$task_tmp/content/blog/invalid/index.en.md" > "$task_tmp/content/blog/invalid/index.la.md"
if build_fixture; then fail 'conflicting publication dates accepted'; fi
case "$(cat "$task_tmp/build.log")" in
    *'translations must share the same publication date'*) ;;
    *) cat "$task_tmp/build.log"; fail 'conflicting dates failed for the wrong reason' ;;
esac
mv "$task_tmp/content/blog/invalid/index.la.md" "$task_tmp/invalid-la.md"
sed '/^date:/d' "$task_tmp/content/blog/invalid/index.en.md" > "$task_tmp/no-date.md"
mv "$task_tmp/no-date.md" "$task_tmp/content/blog/invalid/index.en.md"
if build_fixture; then fail 'missing publication date accepted'; fi
case "$(cat "$task_tmp/build.log")" in
    *'blog posts require a date'*) ;;
    *) cat "$task_tmp/build.log"; fail 'missing date failed for the wrong reason' ;;
esac

printf 'Site checks passed (all 15 language subsets, archives, tags, RSS, drafts, dates, shared URLs, and local links).\n'
