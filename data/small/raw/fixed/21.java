public s2wmp.User getFriend(s2wmp.UserID id) {
    if (friends.containsKey(id.id)) {
        return friends.get(id.id);
    }else {
        return null;
    }
}