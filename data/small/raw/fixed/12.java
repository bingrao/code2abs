public Kubaner.Logic.Subject create(java.lang.String name) {
    Kubaner.Logic.Subject newSubject = new Kubaner.Logic.Subject(name);
    add(newSubject);
    return newSubject;
}