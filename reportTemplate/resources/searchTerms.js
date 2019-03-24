    var thumbnails = document.getElementById("gallery").getElementsByClassName("thumbnail");
    var searchBox  = document.getElementById("searchBox");
    var resultsText = document.getElementById("results");

    function clearSearch()
    {
        for (var i=0; i<thumbnails.length; i++) {
            thumbnails[i].style.display = "block";
        }
        searchBox.value = "";
        resultsText.innerHTML = thumbnails.length + " ocurrences explained";
    }

    function filter(){
        var searchValue = searchBox.value;
        var counter     = 0;

        if (searchValue == "")
        {
            clearSearch();
        }
        else
        {
            for (var i=0; i<thumbnails.length; i++) 
            {
                thumbnails[i].style.display = "none";    
            }
            for (var i=0; i<thumbnails.length; i++) 
            {
                if (thumbnails[i].id.includes(searchValue))
                {
                    counter++;
                    thumbnails[i].style.display="block";
                }    
            }
            resultsText.innerHTML = counter + " ocurrences explained";
        }
    }