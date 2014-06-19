
function jumpPage(newLoc) {

		newPage = newLoc.options[newLoc.selectedIndex].value;

		if (newPage != "") {
		
			window.parent.location.href = newPage
		}
	}

function popUpWindow(URLStr, left, top, width, height){ 
 popUpWindow = open(URLStr, 'popUpWind', 'toolbar=no,location=no,directories=no,status=yes,menubar=no,scrollbars=no,resizable=yes,copyhistory=yes,width='+width+',height='+height+',left='+left+', top='+top+',screenX='+left+',screenY='+top+'');
}  
