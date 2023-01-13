document.addEventListener("DOMContentLoaded", function() {
	const  navBar = document.querySelector(".nav.navbar-nav")
	const hamburgerMenu = document.querySelector(".hamb")
	hamburgerMenu.addEventListener('click', function() {
		navBar.classList.toggle("menu-mobile")
	})
})

document.addEventListener("DOMContentLoaded", function() {	
	var tab = $(".nav.navbar-nav>li>a").parent().addClass("disabled");
	$(function(){
	  $(tab.parent()).on("click", "li.disabled", function(e) {
		e.preventDefault();
		return false;
	  });
	});
	document.getElementById("resetButton").disabled = true;
});

document.addEventListener("DOMContentLoaded", function() {
	$('div.busy').hide()
	setInterval(function(){
	if ($("#confirmationText1").attr('class')=='shiny-text-output noplaceholder shiny-bound-output recalculating') {
		setTimeout(function(){
			if ($("#confirmationText1").attr('class')=='shiny-text-output noplaceholder shiny-bound-output recalculating') {
				$('div.busy').show()
			}
		}, 1000)
	} else {
		$('div.busy').hide()
	}
	}, 100)
}, false);