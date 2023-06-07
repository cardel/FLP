public class Main {
  public static void main(String[] args) {
    Animal perro = new Perro(20, "pepito");
    System.out.println(
      perro.getEdad()+ "  " + perro.getNombre() + "  " + perro.getSonido()  + " " + perro.morir()
    );

    Animal gato = new Gato("manchas", 10);
    System.out.println(
      gato.getEdad()+ "  " + gato.getNombre() + "  " + gato.getSonido() + " " + gato.morir());

    Servivo generico = new Servivo();
    System.out.println(generico.morir());

 
  }
}
