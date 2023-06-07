public class Memoria {
  private int capacidad;
  private String marca;
  private String modelo;

  public Memoria(int capacidad, String marca, String modelo) {
    this.capacidad = capacidad;
    this.marca = marca;
    this.modelo = modelo;
  }

  public int getCapacidad() {
    return capacidad;
  }

  public void setCapacidad(int capacidad) {
    this.capacidad = capacidad;
  }

  public String getMarca() {
    return marca;
  }

  public void setMarca(String marca) {
    this.marca = marca;
  }

  public String getModelo() {
    return modelo;
  }

  public void setModelo(String modelo) {
    this.modelo = modelo;
  }

  
}
