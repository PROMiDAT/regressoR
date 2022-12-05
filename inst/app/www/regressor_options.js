$(document).ready(function() {
 //   document.querySelectorAll("[data-value = \'<span data-id=\"cros\"></span>\']")[0].style.display = "none";

  $(".sidebar").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
  
});