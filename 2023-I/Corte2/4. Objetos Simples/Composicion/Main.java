public class Main {
  public static void main(String[] args) {
    Computador objComputador = new Computador(
      new Memoria(10, "Kingston", "2023"), 
      new DiscoDuro(1000, "Toshiba", "ABC"), 
      new Periferico("Genius","QBV", "Mouse"));

    System.out.println(objComputador.getMemoria().getMarca());
  }


}
