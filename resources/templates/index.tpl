<html>
  <head>
    <script type="text/javascript" src="/jquery.js">
    <title>Learnivore</title>
    <link rel="stylesheet" type="text/css" href="screen.css"/>
  </head>
  <body>
    <div id="content">
      <form action="solve">
      Input an equation to solve: <input type="text" name="lhs" id="lhs"/> = <input type="text" name="rhs" id="rhs"/>
      <input type="button" value="Submit" id="submit"/>
      </form>
    </div>
    <div id="solution">
    </div>
  </body>
  
    <script type="text/javascript">
    $(document).ready(function(){
      $('#submit').click(function(x) {
        $.post('/solveJSON', {"eqn": $('#lhs').val()+"="+$('#rhs').val()}, 
          function(response){
            var solution = "";
            var start = response[0][1];
            var path = response.slice(1, response.length+1);
            for (eq in path) {
              var eqn = path[eq][1].equation;
              solution+= "<br/>=> " + path[eq][0] + "<br/>" + eqn.lhs.expression + " = " + eqn.rhs.expression;
            }
            solution= start.equation.lhs.expression + " = " + start.equation.rhs.expression + solution;
            $('#solution').html(solution);
          }, 'json');
      });
    })
  </script>
</html>
