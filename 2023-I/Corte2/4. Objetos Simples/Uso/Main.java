public class Main {
  public static void main(String[] args) {
    Persona persona = new Persona(30,"Juan","Perez","Masculino","Colombiano","Soltero forever");
    Radio objRadio = new Radio(20, 1905, "El radio de pro juan");

    System.out.println(objRadio.getVolumen());
    persona.subirVolumen(objRadio);
    System.out.println(objRadio.getVolumen());
  }
}
