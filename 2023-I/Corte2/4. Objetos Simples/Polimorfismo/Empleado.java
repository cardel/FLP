public class Empleado extends Persona {
  private String cargo;
  private String empresa;
  private String horario;

  public Empleado(int edad, String nombre, String cargo, String empresa, String horario) {
    super(edad, nombre);
    this.cargo = cargo;
    this.empresa = empresa;
    this.horario = horario;
  }

  public String getCargo() {
    return cargo;
  }

  public void setCargo(String cargo) {
    this.cargo = cargo;
  }

  public String getEmpresa() {
    return empresa;
  }

  public void setEmpresa(String empresa) {
    this.empresa = empresa;
  }

  public String getHorario() {
    return horario;
  }

  public void setHorario(String horario) {
    this.horario = horario;
  }

  public String hablar() {
    return "Estoy trabajando";
  }


}
