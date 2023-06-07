public class Persona {
  private int edad;
  private String nombre;
  private String apellido;
  public Persona(int edad, String nombre) {
    this.edad = edad;
    this.nombre = nombre;
  }

  private String sexo;
  private String nacionalidad;
  private String estadoCivil;

  public Persona(int edad, String nombre, String apellido, String sexo, String nacionalidad, String estadoCivil) {
    this.edad = edad;
    this.nombre = nombre;
    this.apellido = apellido;
    this.sexo = sexo;
    this.nacionalidad = nacionalidad;
    this.estadoCivil = estadoCivil;
  }

  public void subirVolumen(Radio objRadio) {
    objRadio.setVolumen(objRadio.getVolumen() + 1);
  }

  public int getEdad() {
    return edad;
  }

  public void setEdad(int edad) {
    this.edad = edad;
  }

  public String getNombre() {
    return nombre;
  }

  public void setNombre(String nombre) {
    this.nombre = nombre;
  }

  public String getApellido() {
    return apellido;
  }

  public void setApellido(String apellido) {
    this.apellido = apellido;
  }

  public String getSexo() {
    return sexo;
  }

  public void setSexo(String sexo) {
    this.sexo = sexo;
  }

  public String getNacionalidad() {
    return nacionalidad;
  }

  public void setNacionalidad(String nacionalidad) {
    this.nacionalidad = nacionalidad;
  }

  public String getEstadoCivil() {
    return estadoCivil;
  }

  public void setEstadoCivil(String estadoCivil) {
    this.estadoCivil = estadoCivil;
  }

  
}
