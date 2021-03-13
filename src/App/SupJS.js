
exports.cleanInputBox = () => {
        document.getElementById("inp").value = "";
        return 1;
    };
exports.disableInputBox = () => {
        document.getElementById("inp").disabled=true;
           return 1;
       };

 exports.changeParticleSpeed = (speed) => {
     if(speed==null){
        document.getElementById("speedInput").value=1;
        return 1;
     } 
        if(speed>99){
        speed=99;
        }
        document.getElementById("speedInput").value=Math.asin(speed*0.01)*(speed/2);
           return 1;
       };
exports.resizeMagic = (widthAmount) => {
        document.getElementById("magicWidthField").value=widthAmount+50;
           return 1;
       };


