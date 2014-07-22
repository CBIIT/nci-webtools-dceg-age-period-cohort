var app = angular.module("myapp",[]);

app.controller("MyController", function($scope, $http){

        $scope.myForm = {};
        function init(){
            $scope.myForm.ageCriteria = false;
            $scope.myForm.typeCriteria = false;
            $scope.myForm.startShow = false;
            $scope.myForm.quitShow = false;
            $scope.myForm.quitCriteria = false;
            $scope.myForm.packsCriteria = false;
        };
        init();

        $scope.myForm.changeAge = function() {
            if($scope.myForm.age != '' && $scope.myForm.age != undefined){
                $scope.myForm.ageCriteria = !($scope.myForm.age > 54 && $scope.myForm.age < 80);
            }
            else{
                 $scope.myForm.ageCriteria = false;
            }
        }

        $scope.myForm.changeType = function(){
            $scope.myForm.startShow = $scope.myForm.type == 'current';
            $scope.myForm.quitShow = $scope.myForm.type == 'former';
            $scope.myForm.typeCriteria = $scope.myForm.type == 'non';
        }



        $scope.myForm.changeQuit = function(){
            if ($scope.myForm.quit=='') {
                $scope.myForm.quitCriteria = false;
            }
            else {
                $scope.myForm.quitCriteria = ($scope.myForm.age - $scope.myForm.quit > 15);                
            }

        }
        
        $scope.myForm.changePacks = function(){
            if ($scope.myForm.packs=='') {
                $scope.myForm.packsCriteria = false;
            }
            else {
                $scope.myForm.packsCriteria = (($scope.myForm.age - $scope.myForm.start) * $scope.myForm.packs < 30);
            }
        }
        
        $scope.myForm.resetForm = function(){
            init();
            $scope.myForm.age = '';
            $scope.myForm.type = '';
            $scope.myForm.quit = '';
            $scope.myForm.start = '';
            $scope.myForm.packs = '';
            $scope.myForm.group = '';
            $scope.myForm.gender = '';
            $scope.myForm.disease = '';
            $scope.myForm.history = '';   
	    $scope.myForm.gender = '';      
        }
});