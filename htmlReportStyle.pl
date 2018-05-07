%%%%%%%%%%%%%%%%%%%%%%% HEADING %%%%%%%%%%%%%%%%%%%%%%%%%%%%

htmlReportHeading(
'
<!DOCTYPE html>
<html>
<head>

    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <link rel="stylesheet" href=".resources/bootstrap.min.css">    

<style type="text/css">
body {
    background-color: #ffffff;
    min-height: 100vh;
    font: normal 16px sans-serif;
}

.gallery-container h1 {
    text-align: center;
    margin-top: 70px;
    font-family: "Droid Sans", sans-serif;
    font-weight: bold;
    color: #2a2a2a;
}

.gallery-container p.page-description {
    text-align: center;
    margin: 30px auto;
    font-size: 18px;
    color: #2a2a2a;
}

/* Styles for the gallery */

.tz-gallery {
    padding: 40px;
}

.tz-gallery .thumbnail {
    padding: 0;
    margin-bottom: 30px;
    border-style: solid;
    border-width: 1px;
    border-color: #d7cfac;
}

.tz-gallery img {
    border-radius: 2px;
}

.tz-gallery .caption{
    padding: 26px 30px;
    text-align: center;
    border-width: 1px;
    border-color: #fdfee7;
}

.tz-gallery .caption h3 {
    font-size: 14px;
    font-weight: bold;
    margin-top: 0;
}

.tz-gallery .caption p {
    font-size: 12px;
    color: #7b7d7d;
    margin: 0;
}

.baguetteBox-button {
    background-color: transparent !important;
}
</style>

</head>
<body>

<div class="container gallery-container">

    <h1>Explanation Report</h1>

    <p class="page-description text-center">#ResultsNumber#</p> 
    
    <label for="example-text-input" class="col-2 col-form-label">Search terms:</label>

    <div class="input-group">
      <span class="input-group-btn">
        <button class="btn btn-default" type="button" onclick="clearSearch();" tabindex="2" >Clear</button>
      </span>
      <input id="searchBox" type="text" class="form-control" onchange="filter();" tabindex="1" autofocus>
    </div>

    <div id="gallery" class="tz-gallery">

        <div class="row">
').





%%%%%%%%%%%%%%%%%%%%%%%%% ENDING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


htmlReportEnding(
'
        </div>

    </div>

</div>

</body>
<script>
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
</script>
</html>
'
).


%%%%%%%%%%%%%%%%%%%%%%%% IMAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

htmlReportImage(
'
            <div class="col-sm-12 col-md-12" >
                <div id="#Term#" class="thumbnail">
                    <a class="lightbox" target="_blank" href="#ImagePath#">
                        <img src="#ImagePath#" alt="Graph" > 
                    </a>
                    <div class="caption">
                        <h2>#Term#</h2>
                        <p>#TextExplanation#</p>
                    </div>
                </div>
            </div>
'
).