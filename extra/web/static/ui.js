$(document).ready(function(){
  // Substitute async handler for playback
  $(".PlayLink").click(function(event){
      var link = $(this);
      event.preventDefault();
      $.get(this.href, function(){
         parent.header.location.reload();
         /*alert("success!");*/})      
      link.fadeOut('fast', function() {
        link.fadeIn('slow'); });
    })
})


