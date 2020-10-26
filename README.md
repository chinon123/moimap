*Les éléments ne sont pas exhaustifs et la version actuelle peut présenter des erreurs.*

# présentation 

L'objectif de cet exercice est de rassembler l'ensemble des caractéristiques des organismes bailleurs nommés MOI (maîtrise d'ouvrage d'insertation) sur un seul lieu numérique afin de centraliser l'information et de la rendre disponible aux services de l'état. Cet exercice concentre ses efforts sur plusieurs sources afin de consolider l'ensemble des données :
* SIRENE (2020)
* RPLS (2020)
* GEOFLA (2019)
* COG (2019)

L'application focalise son effort sur trois fonctionnalités et combine les éléments cartographiques aux éléments tabulaires. 

# fonction 1 : parc de l'organisme géolocalisé à partir du RPLS 

Je choisis un organisme et je connais les principales caractéristiques de son parc et la localisation de ses logements (RPLS)

# fonction 2 : liste des logements des organismes présents sur un territoire

La réciproque de la fonction 1 

# fonction 3 : territoire d'intervention d'un organisme

Je choisis un organisme et je connais les territoires d'intervention : une MOI est délimitée dans son activité par des agréments pris au niveau de l'administration (dgaln/dhup/lo4) qui délimite les zones d'activité. 

# roadmap 
priorité faible ($) moyenne ($$) élevée ($$$)

fonctionnel : 
* fonction 1 bug pour les régions $$$
* rpls à update 2020 $$
* fonction 3 contrôle de cohérence : multiples anomalies, incohérence des zones illustrées et des logements en fonction $$$

performance : 
* latence perçue leaflet $
* améliorer les liens entre les données : générer des tables intermédiaires ? $$

design :
* fonction 2 : si un territoire choisi alors la zone selectionnée $
