package com.company;

import java.util.*;

public class SocialNetwork
{
    /*
    AF(SN) = <posts <id, <Post>>, network <username, <followedUsers>>, postNumber, influencerLevel> | influencerLevel>0,
             postNumber>=0, posts = {SN.posts<ID, <post[i]>> | SN.containsKey(ID)},
             network = {SN.network<username, <followed[i]>> | SN.containsKey(username)}
    RI = infl_level>0 && postNumber>=0 && posts!=null && network!=null && followersNumber!=null &&
         ∀x ∈ network.keySet() => x != null ∧ x.lenght()>0 &&
         ∀x ∈ posts.keySet() => x != null ∧ x>0 &&
         ∀x ∈ followersNumber.keySet() => x != null ∧ x.lenght()>0 &&
         ∀x,y ∈ network.keySet() => x!=y &&
         ∀x,y ∈ posts.keySet() => x != x!=y &&
         ∀x,y ∈ followersNumber.keySet() => x!=y &&
         ∀x ∈ network.keySet() ∃y ∈ followersNumber.keySet() => x==y
    */

    protected Map<Long,Post> posts;
    protected Map<String, Set<String>> network;
    protected Map<String, Integer> followersNumber;
    protected int influencerLevel;
    protected long postNumber;

    public SocialNetwork(int infl_level)
    {
        posts= new HashMap<>();
        network= new HashMap<>();
        followersNumber= new HashMap<>();
        if(infl_level<=0)
            throw new IllegalArgumentException();
        influencerLevel =infl_level;
        postNumber= 0;
        //eccezione numero negativo
    }
    /*
    REQUIRES: infl_level>0
    MODIFY: this
    EFFECTS: inizializza users e network, assegna infl_level a influencerLevel e 0 a postNumber
    THROWS:
    */

    public Map getNetwork() { return network; }
    public Map getPosts()
    {
        return posts;
    }
    public Map getFollowersNumber()
{
    return followersNumber;
}
    public int getInfluencerLevel()
    {
        return influencerLevel;
    }
    public long getPostNumber()
    {
        return postNumber;
    }


    public Map<String, Set<String>> guessFollowers(List<Post> ps)
    {
        Map<String, Set<String>> res= new HashMap<>();
        for(Post x: ps)
        {
            Set<String> foll= new HashSet<>();
            res.put(x.getAuthor(), foll);
        }

        for(Post x: ps)
        {
            if(x.getText().startsWith("like:"))
            {
                String[] splittedText=x.getText().split(":");
                try
                {
                    long idPost = Integer.parseInt(splittedText[1].trim());
                    for(Post y: ps)
                    {
                        if(y.getID().equals(idPost))
                        {
                            res.get(x.getAuthor()).add(y.getAuthor());
                            break;
                        }
                    }
                }catch(NumberFormatException ignored){}
            }
        }
        return res;
    }
    /*
    restituisce la rete sociale derivata dalla lista di post (parametro del metodo)
    REQUIRES:
    MODIFY:
    EFFECTS: restituisce la rete sociale derivata dalla lista di post (parametro del metodo)
    THROWS:
    */

    public List<String> influencers()
    {
        List<String> influencers = new Vector<>();
        for(String z: followersNumber.keySet())
        {
            if(followersNumber.get(z)>= influencerLevel)
                influencers.add(z);
        }
        return influencers;
    }
    /*
    restituisce gli utenti più influenti delle rete sociale, ovvero quelli che hanno un numero maggiore di "follower"
    REQUIRES:
    MODIFY:
    EFFECTS: restituisce una lista contenente gli utenti tali che il numero di followers è maggiore di influencerLevel
    THROWS:
    */

    public Set<String> getMentionedUsers()
    {
        Set<String> result= new HashSet<>();
        for(Long x: posts.keySet())
                result.add(posts.get(x).getAuthor());
        return result;
    }
    /*
    restituisce l’insieme degli utenti menzionati (inclusi) nei post presenti nella rete sociale
    REQUIRES:
    MODIFY:
    EFFECTS: restituisce un set di stringhe che include tutti gli autori dei post nella rete sociale
    THROWS:
    */

    public Set<String> getMentionedUsers(Set<Post> ps)
    {
        Set<String> result= new HashSet<>();
        for(Post x: ps)
            result.add(x.getAuthor());
        return result;
    }
    /*
    restituisce l’insieme degli utenti menzionati (inclusi) nella lista di post
    REQUIRES:
    MODIFY:
    EFFECTS: restituisce un set di stringhe che include tutti gli autori dei post passati come parametro
    THROWS:
    */

    public Set<Post> writtenBy(String username) throws userNotFound
    {
        if(!network.containsKey(username))
            throw new userNotFound("user "+username+ " not found");
        Set<Post> res= new HashSet<>();
        for(Long x: posts.keySet())
        {
            if(posts.get(x).getAuthor().equals(username))
                res.add(posts.get(x));

        }
        return res;
    }
    /*
    restituisce la lista dei post effettuati dall'utente nella rete sociale il cui nome è dato dal parametro username
    REQUIRES: users.containsKey(username)=true
    MODIFY:
    EFFECTS: restituisce la lista dei post effettuati dall'utente nella rete sociale il cui nome è dato dal parametro username
    THROWS: userNotFound se users.containsKey(username)=false
    */

    public List<Post> writtenBy(List<Post> ps, String username) throws userNotFound
    {
        if(!network.containsKey(username))
            throw new userNotFound("user "+username+ " not found");
        List <Post> post_list = new Vector<>();
        for(Post x: ps)
        {
            if(x.getAuthor().equals(username))
                post_list.add(x);
        }
        return post_list;
    }
    /*
    restituisce la lista dei post effettuati dall'utente il cui nome è dato dal parametro username presenti nella lista ps
    REQUIRES: users.containsKey(username)=true
    MODIFY:
    EFFECTS: restituisce la lista dei post effettuati dall'utente il cui nome è dato dal parametro username presenti nella lista ps
    THROWS: userNotFound se users.containsKey(username)=false
    */

    public List<Post> containing(List<String> words)
    {
        List<Post> result= new Vector<>();
        for(Long x: posts.keySet())
        {
                for(String z: words)
                {
                    if(posts.get(x).getText().contains(z))
                    {
                        result.add(posts.get(x));
                        break;
                    }
                }
        }

        return result;
    }
    /*
    restituisce la lista dei post presenti nella rete sociale che includono almeno una delle parole presenti nella lista delle parole argomento del metodo.
    REQUIRES:
    MODIFY:
    EFFECTS: restituisce la lista dei post presenti nella rete sociale che includono almeno una delle parole presenti nella lista delle parole argomento del metodo.
    THROWS:
    */

    public Set<String> getTaggedUsers()
    {
        Set<String> tagged_users = new HashSet<>();
        for(String x: network.keySet())
        {
            for(Long y: posts.keySet())
            {
                    if(posts.get(y).getText().contains(x))
                        tagged_users.add(x);
            }
        }
        return tagged_users;
    }
    /*
    restituisce l'insieme degli utenti taggati nei post presenti nella rete sociale
    REQUIRES:
    MODIFY:
    EFFECTS:
    THROWS:
    */

    public Set<String> getTaggedUsers(List<Post> ps)
    {
        Set<String> mentioned_users = new HashSet<>();
        for(Post x: ps)
        {
            for(String y: network.keySet())
            {
                if(x.getText().contains(y))
                    mentioned_users.add(y);
            }
        }
        return mentioned_users;
    }
    /*
     restituisce l'insieme degli utenti taggati nella lista di post;
    REQUIRES:
    MODIFY:
    EFFECTS:
    THROWS:
    */

    public long addPost(String T, String A) throws userNotFound, invalidPost, postNotFound
    {
        if(!network.containsKey(A))
            throw new userNotFound("user "+A+ " not found");
        if(T.startsWith("like:"))
        {
            String[] splittedText=T.split(":");
            try
            {
                long idPost = Integer.parseInt(splittedText[1].trim());
                Post likedPost=posts.get(idPost);
                if(likedPost != null)
                {
                    if(likedPost.getText().startsWith("like:"))
                        throw new invalidPost("you can not like a like");
                    if(likedPost.getAuthor().equals(A))
                        throw new invalidPost("you can not like your own post");
                    network.get(A).add(likedPost.getAuthor());
                    Integer value= followersNumber.get(likedPost.getAuthor())+1;
                    followersNumber.replace(likedPost.getAuthor(), value);
                }
                else throw new postNotFound("the post you are trying to like doesn't exist");
            }catch(NumberFormatException ignored){}
        }
        try
        {
            Post post = new Post(postNumber, T, A);
            postNumber++;
            posts.put(postNumber,post);
            return postNumber;
        }
        catch (Exception ex)
        {
            String str= new String(String.valueOf(ex.getStackTrace()));
            throw new invalidPost (str);
        }

    }
    /*
    agginge un Post alla lista di Post dell'utente username se lo username è valido, il post non ha un testo troppo lungo, non si sta cercando di mettere like
    a un proprio post o a un like
    REQUIRES: username chiave valida di users, T.lenght()<140, se è un like non deve essere indirizzato a un proprio post o a un like
    MODIFY: this
    EFFECTS: al set users.get(username).Posts viene aggiunto un post inizializzato con i valori passati per parametro
    THROWS: userNotFound se users.containsKey(username) restituisce false
            invalidPost se si cerca di mettere like a un proprio like o
    */

    public boolean  remove_post (Long I) throws postNotFound
    {
        Post res;
        if(!posts.containsKey(I))
            throw new postNotFound ("not such post with "+I+" was found");
        res=posts.remove(I);
        for(Long x: posts.keySet())
        {
            if (posts.get(x).getText().startsWith("like:"))
            {
                String[] splittedText = res.getText().split(":");
                try
                {
                    long idPost = Integer.parseInt(splittedText[1].trim());
                    if(idPost==res.getID())
                        posts.remove(x);

                }
                catch (NumberFormatException ignored) { }
            }
        }
        return true;
    }
    /*
    rimuove il post postato dall'utente username con ID uguale a I
    REQUIRES: username chiave valida di users, l'oggetto Dati associato alla entry con chiave username ha un Post tale che ID uguale a I
    MODIFY:this
    EFFECTS: rimuove dal set di Post Posts, dall'oggetto Dati associato alla entry con chiave username il Post con ID uguale a I
    THROWS:  userNotFound username chiave non valida di users
             postNotFound l'oggetto Dati associato alla entry con chiave username non ha un Post tale che ID uguale a I
    */

    public String create_account (String username) throws accountAlreadyExists
    {
        if(network.containsKey(username))
            throw new accountAlreadyExists("account "+username+" already exists");
        Set<String> foll= new HashSet<>();
        network.put(username, foll);
        followersNumber.put(username, 0);
        return username;
    }
    /*
    aggiunge un nuovo utente al social network e restituisce lo username
    REQUIRES: username non attualmente mappato
    MODIFY: this
    EFFECTS: aggiunge la entry con chiave username alla mappa user e restituisce lo username
    THROWS: accountAlreadyExists se l'entry username è attualmente già mappata
    */

    public boolean follow(String username, String followed) throws userNotFound
    {
        boolean res;
        if(network.get(username) == null)
            throw new userNotFound("user "+username+ " not found");
        if(network.get(followed) == null)
            throw new userNotFound("user "+username+ " not found");
        res=network.get(username).add(followed);
        Integer val= followersNumber.get(followed)+1;
        followersNumber.put(followed,val);
        return res;
    }
    /*
    aggiunge a network.get(username) la stringa followed se non è già presente e restituisce true, false altrimenti
    REQUIRES: network.containsKey(username) true e network.containsKey(followed) true
    MODIFY: this
    EFFECTS: aggiunge la stringa followed alla lista di stringhe network.get(username)
    THROWS: userNotFound se users.get(username) restituisce false
            userNotFound se users.get(followed) restituisce false
    */

    public boolean unfollow(String username, String unfollowed) throws userNotFound
    {
        boolean res;
        if(network.get(username) == null)
            throw new userNotFound("user "+username+ " not found");
        if(network.get(unfollowed) == null)
            throw new userNotFound("user "+username+ " not found");
        res=network.get(username).remove(unfollowed);
        Integer val= followersNumber.get(unfollowed)-1;
        followersNumber.put(unfollowed,val);
        return res;
    }

    /*
    rimuove da network.get(username) la stringa followed se è presente e restituisce true, false altrimenti
    REQUIRES: network.containsKey(username) true e network.containsKey(followed) true
    MODIFY: this
    EFFECTS: aggiunge la stringa followed alla lista di stringhe network.get(username)
    THROWS: userNotFound se users.get(username) restituisce false
            userNotFound se users.get(followed) restituisce false
    */


}