# 📝 Formulario GURI

<p align="left">
  <img src="https://www.r-project.org/logo/Rlogo.png" alt="Logo R" width="40"/>
  <img src="https://shiny.rstudio.com/images/shiny.png" alt="Logo Shiny" width="40"/>
</p>


Repositorio con un **prototipo del formulario GURI**. Incluye modo *usuario* y *administrador*, subida de imagenes e identificación de usuarios vía `user_id` en la URL.

---

## 🚀 Funcionalidades principales

### 👤 Modo Usuario
- Completar el formulario.  
- Adjuntar imágenes y archivos.  
- Editar respuestas luego de enviarlas (recargando la página).

### 🛠 Modo Administrador
- Ver todas las respuestas enviadas.  
- Descargar respuestas en CSV.  
- Acceso protegido por contraseña (configurable).

---

## 💻 Requisitos

- R (recomendado >= 4.0)  
- Paquetes R: `shiny`, `DBI`, `RSQLite`, `dplyr`, `bslib`

Instalación rápida de paquetes (ejemplo):

```r
install.packages(c("shiny","DBI","RSQLite","dplyr","bslib"))
```

---

## ▶️ Cómo ejecutar (local)

1. Clonar el repositorio:

```bash
git clone https://github.com/fran-duarte02/FormularioGURI.git
```

2. Ejecutar la app desde R:

```r
# desde la consola
shiny::runApp("app.R")
# o abrir app.R y correrlo desde el boton de Run App
```

3. Se abrira el navegador al que habra que modificarle la URL, segun la necesidad (se describe a continuacion).

---

## 🔑 Modos de acceso (parámetros URL)

- **Administrador**
  - URL(ejemplo, obiamente el numero de IP va a variar): `http://127.0.0.1:6196/?admin=1`
  - Contraseña por defecto (prototipo): `PISAURUGUAY2025`  

- **Usuario**
  - URL (ejemplo, obiamente el numero de IP va a variar): `http://127.0.0.1:6196/?user_id=1234`
  - Reemplazar `1234` por el ID que identifica al usuario.  
  - Al acceder con `user_id`, se despliega el formulario vinculado a esa persona.  
  - Permite subir imágenes y editar lo ya enviado (recargar la página para editar).

> 💡 Como realmente funcionaria desde la plataforma GURI: se genera el enlace con `/?user_id=<ID>` para identificar automáticamente al usuario que hace clic.

---

## 📬 Contacto

Para dudas o pruebas del flujo: **Fran**
