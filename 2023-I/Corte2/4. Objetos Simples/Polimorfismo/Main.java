public class Main {
  public static void main(String[] args) {
    Persona persona = new Empleado( 30, "Pedro", "Gerente", "Univalle", "Nunca va");

    System.out.println(persona.hablar());
    System.out.println(persona.hablar("Hola"));
  }
}
