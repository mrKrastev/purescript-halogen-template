
exports.cleanInputBox = () => {
        document.getElementById("inp").value = "";
        return 1;
    };
 exports.disableInputBox = () => {
        document.getElementById("inp").disabled=true;
           return 1;
       };

 exports.changeParticleSpeed = (speed) => {
        document.getElementById("inputSpeed").value=speed;
           return 1;
       };



