public class Radio {
  private int volumen;
  private int estacion;
  private String name;

  public Radio(int volumen, int estacion, String name) {
    this.volumen = volumen;
    this.estacion = estacion;
    this.name = name;
  }

  public int getVolumen() {
    return volumen;
  }

  public void setVolumen(int volumen) {
    this.volumen = volumen;
  }

  public int getEstacion() {
    return estacion;
  }

  public void setEstacion(int estacion) {
    this.estacion = estacion;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  
}
