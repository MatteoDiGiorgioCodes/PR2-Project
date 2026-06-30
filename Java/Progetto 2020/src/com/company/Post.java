package com.company;

import java.sql.Timestamp;

public class Post
{
    /*
    AF(P) = <ID,text,author,publication_date>
    RI = text!=null && ID!=null && author!=null
    */
    private Long ID;
    private String text;
    private String author;
    private Timestamp publication_date;

    public Post(Long I, String T, String A)
    {

        if(T.length()>140)
            throw new IllegalArgumentException("Too long text");
        text=T;
        author=A;
        publication_date= new Timestamp(System.currentTimeMillis());
        ID=I;

    }

    public String getText()
    {
        return text;
    }
    public Long getID() { return ID; }
    public String getAuthor()
    {
        return author;
    }
    public Timestamp getPublication_date()
    {
        return publication_date;
    }

}
