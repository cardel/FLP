public class Computador {
  private Memoria memoria;
  private DiscoDuro discoDuro;
  private Periferico periferico;

  public Computador(Memoria memoria, DiscoDuro discoDuro, Periferico periferico) {
    this.memoria = memoria;
    this.discoDuro = discoDuro;
    this.periferico = periferico;
  }

  public Memoria getMemoria() {
    return memoria;
  }

  public void setMemoria(Memoria memoria) {
    this.memoria = memoria;
  }

  public DiscoDuro getDiscoDuro() {
    return discoDuro;
  }

  public void setDiscoDuro(DiscoDuro discoDuro) {
    this.discoDuro = discoDuro;
  }

  public Periferico getPeriferico() {
    return periferico;
  }

  public void setPeriferico(Periferico periferico) {
    this.periferico = periferico;
  }

  
}
