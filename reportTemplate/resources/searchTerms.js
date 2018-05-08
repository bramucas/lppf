    var thumbnails = document.getElementById("gallery").getElementsByClassName("thumbnail");
    var searchBox  = document.getElementById("searchBox");

    function clearSearch()
    {
        for (var i=0; i<thumbnails.length; i++) {
            thumbnails[i].style.display = "block";
        }
        searchBox.value = "";
    }

    function filter(){
        var searchValue = searchBox.value;

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
                    thumbnails[i].style.display="block";
                }    
            }
        }
    }