# Kiss

Kiss is a programation langage used to demonstrate how to create your own langage 

Example :

    var main= {}
    main.Run = fun() -> 
    {
    	var console = use(Core.Console)
    	var rand = use (Core.Random)
    	var ninja = { Life = 3 }
    	var badNinja = { Life = 3 }
    	
    	ninja.IsAlive = fun(n) -> n.Life > 0
    	ninja.Sword = fun(n) -> n.Life--
    	
    	while(badninja.IsAlive() && ninja.IsAlive())
    	{
    	    if(rand.Next(100)>50)
    	    {
    	  			Ninja.Sword(badNinja)
    	  	}
    	  	else
    	  	{
    	  		Badninja.Sword(ninja)
    	  	}
    	  
    	  	console.Write(ninja.Life)
    	  	console.Write(" - ")
    	  	console.Write(badNinja.Life)
    	}
    }
