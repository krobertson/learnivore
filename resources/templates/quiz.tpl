<html>
  <head>
    <script type="text/javascript" src="/jquery.js">
    <title>Learnivore</title>
    <link rel="stylesheet" type="text/css" href="screen.css"/>
  </head>
  <body>
    <div id="question">
      <input type="text" name="lhs" id="lhs"/> = <input type="text" name="rhs" id="rhs"/>
    </div>
    <div id="proposed_solution">
      Enter a Solution:
      <br/>
      <form action="solve" id='solution_form'>
      </form>
    </div>
    <div id="solution">
    </div>
    <input type='button' id='add' value='Add Intermediate Solution'/>
    <input type='button' id='verify' value='Check Answer'/>
    <input type='button' id='solve' value='Give Up'/>
    <input type='button' id='next' value='Skip'/>
  </body>
  
    <script type="text/javascript">
    $(document).ready(function(){
      step = 0;
      
      $('#add').click(function(x) {
        step = step + 1;
        $('#solution_form').append(' =>');
        addIntermediateSolution(step);
      });
      
      $('#verify').click(function(x) {
        $.post('/verifyJSON', {"eqn": $('#lhs').val()+"="+$('#rhs').val()}, solveJSON, 'json');
      });
      
      $('#solve').click(function(x) {
        $.post('/solveJSON', {"eqn": $('#lhs').val()+"="+$('#rhs').val()}, solveJSON, 'json');
      });
      
      $('#next').click(function(x){
        getQuestion();
      });
      
      var getQuestion = function() {
        $.post('/getQuestionJSON', {}, nextQuestion, 'json');
        return true;
      };
      
      var nextQuestion = function(response) {
        alert('in here');
        console.log(response);
        var lhs = response.question && response.question.lhs;
        var rhs = response.question && response.question.rhs;
        console.log(lhs);
        console.log(rhs);
        var html = '<input type="text" value="' + lhs + '" name="l" id="lhs"> = <input type="text" value="' + rhs + '" name="r" id="rhs">';
        $('#question').html(html);
        $('#solution_form').html('');
        step = 0;
        addIntermediateSolution(step);
        return true;
      };
      
      var addIntermediateSolution = function(i) {
        var html = '<br/><input type="text" name="lhs'+ i + '" class="lhs"/> = <input type="text" name="rhs'+ i + '" class="rhs"/>';
        $('#solution_form').append(html);
        return true;
      };
      
      var solveJSON = function(response){
        var solution = "";
        var start = response[0][1];
        var path = response.slice(1, response.length+1);
        for (eq in path) {
          var eqn = path[eq][1].equation;
          solution+= "<br/>=> " + path[eq][0] + "<br/>" + eqn.lhs.expression + " = " + eqn.rhs.expression;
        }
        solution= start.equation.lhs.expression + " = " + start.equation.rhs.expression + solution;
        $('#solution').html("Solution:<br/>");
        $('#solution').append(solution);
        return true;
      };
      
      var verifyJSON = function(response) {
        alert('hoohaa');
        return true;
      };
      
      getQuestion();
      addIntermediateSolution(step);
      
    })
  </script>
</html>
