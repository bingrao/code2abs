@java.lang.Override
@javax.transaction.Transactional
public void update(java.lang.Long id) {
    com.example.model.User user = entityManager.find(com.example.model.User.class, id);
}