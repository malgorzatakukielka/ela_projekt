$(document).on('click', '.delete-btn', function() {
    var row = $(this).data('row');
    Shiny.setInputValue('delete_row', row, {priority: 'event'});
});
