package com.company;

import java.util.List;
import java.util.Set;
import java.util.Vector;

public class Main {

    public static void main(String[] args) throws Exception {
        SocialNetwork SN= new SocialNetwork(3);
        var a = SN.create_account("Leslie");
        var b = SN.create_account("Ron");
        var c = SN.create_account("Tom");
        var d = SN.create_account("Andy");
        var e = SN.create_account("April");
        var f = SN.create_account("Donna");
        var g = SN.create_account("Jerry");
        var h = SN.create_account("Ann");
        var i = SN.create_account("Ben");
        var j = SN.create_account("Chris");

        SN.addPost("Ciao a tutti!",a);
        SN.addPost("like:1",b);
        SN.addPost("like:1",i);
        SN.addPost("like:1",h);
        SN.addPost("like:1",e);

        SN.addPost("Amo i parchi!",a);
        SN.addPost("anche io amo i parchi!",g);
        long p= SN.addPost("Non importa a nessuno cosa pensi tu Jerry",c);
        SN.addPost("like:"+p,d);

        long p2= SN.addPost("Amo andare a correre nei parchi!",j);
        SN.addPost("like:"+p2,f);
        SN.addPost("like:"+p2,a);
        SN.addPost("like:"+p2,h);
        SN.addPost("like:"+p2,d);

        List<String> list= new Vector<>();
        list.add("parchi");

        List<Post> list2= SN.containing(list);
        for(Post x: list2)
        {
            System.out.println(x.getText()+" "+x.getAuthor());
        }

        List<String> list3= SN.influencers();
        System.out.println("influencers are");
        for(String x: list3)
        {
            System.out.println(x);
        }

        Set<String> list4= SN.getMentionedUsers();
        System.out.println("mentioned users are");
        for(String x: list4)
        {
            System.out.println(x);
        }

        Set<String> list5= SN.getTaggedUsers();
        System.out.println("tagged users are");
        for(String x: list5)
        {
            System.out.println(x);
        }

        try
        {
            SN.addPost("like:44444", "Tom");
        }
        catch (Exception ex)
        {
            ex.printStackTrace( );
        }

        try
        {
            SN.addPost("like:2", "Tom");
        }
        catch (Exception ex)
        {
            ex.printStackTrace( );
        }

        try
        {
            SN.addPost("Hi Ron", "Tammy");
        }
        catch (Exception ex)
        {
            ex.printStackTrace( );
        }

        try
        {
            SN.create_account("Ron");
        }
        catch (Exception ex)
        {
            ex.printStackTrace( );
        }


        String end= "end";
        System.out.println(end);
    }
}
