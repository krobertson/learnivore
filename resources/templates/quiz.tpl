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
        var fn = function(selector) {
          return $(selector).map(function(i,x) {return $(x).val()}).get();
        };
        var fn2 = function(list1, list2) {
          var ret_string="";
          if(list1.length > list2.length) {
            for(i in list2) {
              if (i > 0) {ret_string+=","+list1[i]+"="+list2[i]}
              else {ret_string+= list1[i]+"="+list2[i]}
            }
          }
          else {
            for(i in list1) {
              if (i > 0) {ret_string+=","+list1[i]+"="+list2[i]}
              else {ret_string+= list1[i]+"="+list2[i]}
            }
          }
          return ret_string;
        }
        
        
        $.post('/verifyAnswerJSON', 
              {"question": $('#lhs').val()+"="+$('#rhs').val(), 
               "solution": fn2(fn('.lhs'), fn('.rhs'))}, 
               verifyAnswerJSON, 
               'json');
      });
      
      $('#solve').click(function(x) {
        $.post('/solveJSON', {"eqn": $('#lhs').val()+"="+$('#rhs').val()}, solveJSON, 'json');
      });
      
      $('#next').click(function(x){
        init();
      });
      
      var verifyAnswerJSON = function(response) {
        renderSolution(response);
        return true;
      };
      
      var init = function() {
        step = 0;
        getQuestion();
        $('#solution').html('');
      };
      
      var nextQuestion = function(response) {
        var lhs = response.question && response.question.lhs;
        var rhs = response.question && response.question.rhs;
        var html = '<input type="text" value="' + lhs + '" name="l" id="lhs"> = <input type="text" value="' + rhs + '" name="r" id="rhs">';
        $('#question').html(html);
        $('#solution_form').html('');
        step = 0;
        addIntermediateSolution(step);
        return true;
      };
      
      var getQuestion = function() {
        $.post('/getQuestionJSON',{}, nextQuestion, 'json');
        return true;
      };
      
      var addIntermediateSolution = function(i) {
        var html = '<br/><input type="text" name="lhs['+ i + ']" class="lhs"/> = <input type="text" name="rhs['+ i + ']" class="rhs"/>';
        $('#solution_form').append(html);
        return true;
      };
      
      var solveJSON = function(response){
        renderSolution(response);
        return true;
      };
      
      var renderSolution = function(solution) {
        var solution_string = "";
        var start = solution[0][1];
        var path = solution.slice(1, solution.length+1);
        for (eq in path) {
          var eqn = path[eq][1].equation;
          solution_string+= "<br/>=> " + path[eq][0] + "<br/>" + eqn.lhs.expression + " = " + eqn.rhs.expression;
        }
        solution_string = start.equation.lhs.expression + " = " + start.equation.rhs.expression + solution_string;
        $('#solution').html("Solution:<br/>");
        $('#solution').append(solution_string);
        return true;
      }
      
      init();
      
    })
  </script>
</html>
