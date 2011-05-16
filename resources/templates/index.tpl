<html>
  <head>
    <script type="text/javascript" src="/jquery.js">
    <title>Learnivore</title>
    <link rel="stylesheet" type="text/css" href="screen.css"/>
  </head>
  <body>
    <div id="content">
      <form action="solve">
      Input an equation to solve: <input type="text" name="lhs" id="lhs"> = <input type="text" name="rhs" id="rhs">
      <input type="button" value="Submit" id="submit"/>
      </form>
    </div>
    <div id="solution">
    </div>
  </body>
  
    <script type="text/javascript">
    $(document).ready(function(){
      $('#submit').click(function(x) {
        $.post('/solve', {"eqn": $('#lhs').val()+"="+$('#rhs').val()}, 
          function(response){
            var solution = response.replace(/=&gt;/g, "<br/>=&gt;<br/>");
            $('#solution').html(solution);
          });
      });
    })
  </script>
</html>
