public class Gato extends Animal{
  Gato(String nombre, int edad) {
    this.nombre = nombre;
    this.edad = edad;
    this.sonido = "Miau";
  }

  @Override
  public String getSonido() {
    return "Soy un sonido modificado "+this.sonido;
  }
}
