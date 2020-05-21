public Kubaner.Logic.Subject create(java.lang.String name) {
    Kubaner.Logic.Subject newSubject = new Kubaner.Logic.Subject(name);
    subjects.add(newSubject);
    return newSubject;
}