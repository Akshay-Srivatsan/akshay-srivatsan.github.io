$('.scrollspy').scrollSpy({
    getActiveElement: function(name) {
        return $('a[href="#' + name + '"]').parent().get();
    },
    scrollOffset: 50
});
$('.pin').pushpin();
$('.modal').modal();
$('.collapsible').collapsible();