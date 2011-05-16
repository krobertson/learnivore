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
        $.post('/solveJSON', {"eqn": $('#lhs').val()+"="+$('#rhs').val()}, 
          function(response){
            var solution = ""
            var answer = response[response.length-1];
            var path = response.slice(0, response.length-1);
            for (eq in path) {
              var eqn = response[eq].equation;
              solution+= eqn.lhs.expression + " = " + eqn.rhs.expression + "<br/>=><br/>"
            }
            solution+= answer.equation.lhs.expression + " = " + answer.equation.rhs.expression;
            $('#solution').html(solution);
          }, 'json');
      });
    })
  </script>
</html>
