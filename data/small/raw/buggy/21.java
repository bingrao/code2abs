public s2wmp.User getFriend(s2wmp.UserID id) {
    if (friends.containsKey(id)) {
        return friends.get(id);
    }else {
        return null;
    }
}