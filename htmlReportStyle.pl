%%%%%%%%%%%%%%%%%%%%%%% HEADING %%%%%%%%%%%%%%%%%%%%%%%%%%%%

htmlReportHeading(
'
<!DOCTYPE html>
<html>
<head>

    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
    <link href="https://fonts.googleapis.com/css?family=Droid+Sans:400,700" rel="stylesheet">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/baguettebox.js/1.8.1/baguetteBox.min.css">

<style type="text/css">
body {
    background-color: #beb693;
    min-height: 100vh;
    font: normal 16px sans-serif;
}

.gallery-container h1 {
    text-align: center;
    margin-top: 70px;
    font-family: "Droid Sans", sans-serif;
    font-weight: bold;
    color: #ffffff;
}

.gallery-container p.page-description {
    text-align: center;
    margin: 30px auto;
    font-size: 18px;
    color: #ffffff;
}

/* Styles for the gallery */

.tz-gallery {
    padding: 40px;
}

.tz-gallery .thumbnail {
    padding: 0;
    margin-bottom: 30px;
    border-style: solid;
    border-width: 9px;
    border-color: #d7cfac;
}

.tz-gallery img {
    border-radius: 2px;
}

.tz-gallery .caption{
    padding: 26px 30px;
    text-align: center;
    border-width: 1px;
    border-color: #d7cfac;
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
    
    <div class="tz-gallery">

        <div class="row">
').





%%%%%%%%%%%%%%%%%%%%%%%%% ENDING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


htmlReportEnding(
'
        </div>

    </div>

</div>

<script src="https://cdnjs.cloudflare.com/ajax/libs/baguettebox.js/1.8.1/baguetteBox.min.js"></script>
<script>
    baguetteBox.run(".tz-gallery");
</script>
</body>
</html>
'
).


%%%%%%%%%%%%%%%%%%%%%%%% IMAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

htmlReportImage(
'
            <div class="col-sm-12 col-md-12" >
                <div class="thumbnail">
                    <a class="lightbox" href="#ImagePath#">
                        <img src="#ImagePath#" alt="Graph">
                    </a>
                    <div class="caption">
                        <h2>#Term#</h2>
                        <p>#TextExplanation#</p>
                    </div>
                </div>
            </div>
'
).