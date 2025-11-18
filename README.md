# Replicación y Análisis de Discriminación Étnica en el Mercado Sueco de Alquiler  
## Correspondence Experiment + Power Simulations

Este repositorio contiene todo el material empírico, de simulación y de documentación desarrollado para el análisis de discriminación étnica en el mercado sueco de alquiler, inspirado en el experimento clásico de Ahmed & Hammarstedt (2008). El objetivo central es evaluar si nombres asociados a distintos orígenes étnicos reciben un trato diferenciado por parte de arrendadores en plataformas digitales de vivienda.

---

## 📌 Contenido del repositorio

### **1. Código de simulaciones (`R/`)**
Incluye todas las funciones y scripts utilizados para:

- Construir el *data generating process* (DGP) basado en las probabilidades reales del experimento.
- Simular resultados potenciales con ruido no observado.
- Estimar modelos lineales de probabilidad (LPM) en cada réplica.
- Calcular:
  - Poder estadístico para distintos tamaños muestrales y niveles de ruido.
  - Distribuciones empíricas del estadístico *t* bajo H0 y H1.
  - Minimum Detectable Effect (MDE).
  - Sesgo y cobertura del estimador.
  - Poder para detectar heterogeneidad según el origen del arrendador.
- Guardar y exportar automáticamente todas las figuras (PDF).

### **2. Resultados y figuras (`Results/`)**
Contiene todos los gráficos generados por el código, entre ellos:

- Curvas de poder por nivel de ruido.
- Distribuciones empíricas del estadístico *t*.
- Curvas del MDE.
- Sesgo y cobertura del estimador.
- Poder para heterogeneidad bajo distintos escenarios.

Todos los gráficos están en formato PDF, listos para incluir en un paper o informe.

### **3. Documento principal (`Paper/`)**
Incluye:

- La sección metodológica completa en LaTeX.
- Especificación econométrica totalmente pre-especificada.
- Tablas de variables y definiciones.
- Discusión detallada de simulaciones, sesgo, cobertura y poder.
- Consideraciones sobre validez del diseño, limitaciones y trade-offs.
- Redacción final de secciones clave para la versión del paper en PDF.

---

## 🎯 Objetivo del proyecto

El propósito de este repositorio es:

1. Replicar y actualizar la evidencia del experimento original sobre discriminación en el mercado de vivienda en Suecia.
2. Evaluar rigurosamente la capacidad del diseño experimental para detectar efectos de discriminación, utilizando simulaciones extensivas.
3. Documentar un plan de especificación estadística transparente, reproducible y alineado con estándares de investigación.
4. Generar herramientas para comprender:
   - qué tamaño de muestra se necesita,
   - qué nivel de ruido afecta la potencia,
   - cuándo se pueden detectar heterogeneidades,
   - y cuán confiable es el estimador.

---

## 🧪 Metodología resumida

- Se envían solicitudes idénticas que solo varían en el nombre del remitente (Erik, Maria, Mohammed).
- Las respuestas se modelan con un LPM para interpretar los efectos en puntos porcentuales.
- Se construye un DGP que replica probabilidades reales del artículo y añade ruido normal idiosincrático.
- Cada réplica produce una base artificial sobre la que se estima el contraste Mohammed–Erik.
- Se realizan cientos de simulaciones por combinación de parámetros para medir desempeño estadístico.

---

## 🔍 Contribución

Este repositorio permite:

- Entender la persistencia de la discriminación en mercados de vivienda.
- Evaluar la robustez de diseños experimentales de correspondencia.
- Identificar limitaciones estadísticas de detectar heterogeneidad.
- Incluir todas las simulaciones necesarias para replicabilidad completa del análisis.

---

## 📄 Autores

David Flórez López  
Daniel Hernández Leguía  
Universidad de los Andes, 2025.

---

## 🤝 Licencia y uso

Este repositorio está disponible únicamente para fines académicos, de investigación y docencia.  
Se prohíbe el uso de nombres ficticios en contextos reales fuera de experimentos aprobados.

