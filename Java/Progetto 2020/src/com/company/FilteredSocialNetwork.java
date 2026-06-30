package com.company;

import java.util.ArrayList;

public class FilteredSocialNetwork extends SocialNetwork
{
    ArrayList<String> banlist;

    public FilteredSocialNetwork(int infl_level, ArrayList<String> toBeBanned)
    {
        super(infl_level);
        banlist= toBeBanned;
    }

    public long addPost(String T, String A) throws userNotFound, invalidPost, postNotFound
    {
        if(!network.containsKey(A))
            throw new userNotFound("user "+A+ " not found");
        for(String x: banlist)
        {
            if(T.contains(x))
            {
                throw new invalidPost ("your post contains the string "+x+" which is banned");
            }
        }
        if(T.startsWith("like:"))
        {
            String[] splittedText=T.split(":");
            try
            {
                long idPost = Integer.parseInt(splittedText[1].trim());
                Post likedPost=this.posts.get(idPost);
                if(likedPost != null)
                {
                    if(likedPost.getText().startsWith("like:"))
                        throw new invalidPost("you can not like a like");
                    network.get(likedPost.getAuthor()).add(A);
                    Integer value= followersNumber.get(likedPost.getAuthor())+1;
                    followersNumber.replace(likedPost.getAuthor(), value);
                }
                else throw new postNotFound("the post you are trying to like doesn't exist");
            }catch(NumberFormatException ignored){}
        }
        postNumber++;
        Post post=new Post(postNumber, T, A);
        posts.put(postNumber,post);
        return postNumber;
    }
    /*
    agginge un Post alla lista di Post dell'utente username se lo username è valido e non contiene termini vietati
    REQUIRES: username chiave valida di users
    MODIFY: this
    EFFECTS: al set users.get(username).Posts viene aggiunto un post inizializzato con i valori passati per parametro
    THROWS: userNotFound se users.containsKey(username) restituisce false
            invalidPost
    */
}
