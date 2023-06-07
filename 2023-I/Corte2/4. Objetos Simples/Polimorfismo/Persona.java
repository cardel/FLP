public abstract class Persona {
  
  private int edad;
  private String nombre;

  public Persona(int edad, String nombre) {
    this.edad = edad;
    this.nombre = nombre;
  }

  public String hablar() {
    return "Hola que tal";
  }

  public String hablar(String frase) {
    return "Soy un método recargado "+frase;
  }
}
