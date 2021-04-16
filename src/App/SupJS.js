
exports.cleanInputBox = () => {
        document.getElementById("inp").value = "";
        return 1;
    };
exports.disableInputBox = () => {
        document.getElementById("inp").disabled=true;
           return 1;
       };
exports.startGame = () => {
        document.getElementById("inp").disabled=false;
        document.getElementById("initialTimer").style.visibility=false;
           return 1;
       };

 exports.changeParticleSpeed = (speed) => {
     if(speed==null){
        document.getElementById("speedInput").value=3;
     }else{
        if(speed>99){
            speed=99;
            }
        if(speed<3){
             speed=3;
        }
    document.getElementById("speedInput").value=3+Math.asin(speed*0.01)*(speed/2);
     }
    };
exports.changeParticleSpeedandWidth = (speed,widthAmount) => {
    if(speed==null){
        document.getElementById("speedInputp2").value=3;
     }else{
        if(speed>99){
            speed=99;
            }
        if(speed<3){
             speed=3;
        }
    document.getElementById("speedInputp2").value=3+Math.asin(speed*0.01)*(speed/2);
     } 
    document.getElementById("magicWidthField").value=widthAmount+50;
    return 1;
 };
exports.changeParticleSpeed2 = (speed) => {
        if(speed==null){
           document.getElementById("speedInputp2").value=3;
        }else{
           if(speed>99){
               speed=99;
               }
           if(speed<3){
                speed=3;
           }
       document.getElementById("speedInputp2").value=3+Math.asin(speed*0.01)*(speed/2);
        } 
    };   
exports.resizeMagic = (widthAmount) => {
        document.getElementById("magicWidthField").value=widthAmount;
           return 1;
       };

exports.renderMagicJs = () => {
        document.getElementById("particles-js").style.visibility="visible";
        return 1;
       };
exports.renderMagicJsPVP = () => {
        document.getElementById("particles-js").style.visibility="visible";
        document.getElementById("particles-jsp2").style.visibility="visible";
        return 1;
       };
exports.fixPVPmagicPositioning = () => {
    window.alert("aha");
        var magic1 =document.getElementById("particles-js");
        magic1.style.left="400px";
        magic1.style.top="300px";
        var magic2=document.getElementById("particles-jsp2");
        magic2.style.left="800px";
        magic2.style.top="300px";
        return 1;
       };
exports.fixPVPmagic = () => {
    document.getElementById("inp").disabled=true;
    document.getElementById("particles-jsp2").style.visibility="visible";
    document.getElementById("particles-js").style.visibility="visible";
    document.getElementById("particles-js").style.position="relative";
    document.getElementById("particles-jsp2").style.position="relative";
           var magic1 =document.getElementById("particles-js");
            var magic2=document.getElementById("particles-jsp2");
            magic1.style.top="";
            magic1.style.left="";
            magic2.style.right="0px";
            document.getElementById("magicfield").style.position="absolute";
            document.getElementById("magicfield").style.top="30%";
            document.getElementById("magicfield").style.left="27%";
            document.getElementById("magicfield").style.height="200px";
            document.getElementById("magicfield").style.width="48%";


            return 1;
           };
