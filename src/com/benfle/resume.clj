(ns com.benfle.resume
  "A set of functions to generate my résumé at http://benfle.com/resume"
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.data.codec.base64 :as base64]
            [hiccup.core :refer [html]]
            [clojure.java.shell :refer [sh]]))

;; Specs

(s/def ::recommendation (s/keys :req [:recommendation/text
                                      :recommendation/author]))
(s/def :recommendation/text string?)
(s/def :recommendation/author string?)

(s/def ::affiliation (s/keys :req [:affiliation/name
                                   :affiliation/url
                                   :affiliation/image]))

(s/def ::work (s/keys :req [:work/period
                            :work/title
                            :work/projects]
                      :opt [:work/affiliation
                            :work/recommendations]))
(s/def :work/period (s/cat :start pos-int?
                           :end pos-int?))
(s/def :work/title string?)
(s/def :work/affiliation ::affiliation)
(s/def :work/projects (s/coll-of string?))
(s/def :work/recommendations (s/coll-of ::recommendation))

(s/def ::education (s/keys :req [:education/affiliation
                                 :education/diploma]))
(s/def :education/affiliation ::affiliation)
(s/def :education/diploma string?)

(s/def ::resume (s/keys :req [:resume/name
                              :resume/tagline
                              :resume/notable-affiliations
                              :resume/location
                              :resume/specialties
                              :resume/goal
                              :resume/experience
                              :resume/education]))
(s/def :resume/name string?)
(s/def :resume/tagline string?)
(s/def :resume/notable-affiliations (s/coll-of string?))
(s/def :resume/location string?)
(s/def :resume/specialties (s/coll-of string?))
(s/def :resume/goal (s/coll-of string?))
(s/def :resume/experience (s/coll-of ::work))
(s/def :resume/education (s/coll-of ::education))

;; Publishing

(defn image-mime-type
  "Try to guess the mime-type from the image's filename suffix."
  [image]
  (let [mime-type (condp #(.endsWith %2 %1) image
                    ".jpg" "image/jpeg"
                    ".png" "image/png"
                    ".svg" "image/svg+xml"
                    ::unknown)]
    (when (= mime-type ::unknown)
      (throw (Exception. (str "Unknown image suffix: " image))))
    mime-type))

(defn inline-image
  "Convert the image into a base64 encoded string."
  [image]
  (let [mime-type (image-mime-type image)
        input (-> image io/resource io/input-stream)
        output (java.io.ByteArrayOutputStream.)]
    (base64/encoding-transfer input output)
    (str "data:" mime-type ";base64," (.toString output))))

(defn render-work
  [{:keys [work/period work/title work/projects work/affiliation work/recommendations]}]
  (let [{:keys [start end]} period
        {:keys [affiliation/name affiliation/url affiliation/image]} affiliation]

    [:section.work

     [:div.image
      (when image
        [:img {:src (inline-image (str "images/" image))
               :alt name}])]

     [:div.text

      [:h3 title]

      (when name
        [:p.affiliation
         (if url
           [:a {:href url} name]
           name)])

      [:p.period
       (if (= start end)
         start
         (str start " — " end))]

      [:div.columns

       (into [:ul.projects
              (map (fn [description]
                     [:li.project [:p description]])
                   projects)])

       (into [:ul.recommendations]
             (map (fn [{:keys [recommendation/text recommendation/author]}]
                    [:li
                     [:p text]
                     [:p (str " — " author)]])
                  recommendations))]]]))

(defn render-education
  [{:keys [education/affiliation education/diploma]}]
  (let [{:keys [affiliation/name affiliation/url affiliation/image]} affiliation]
    [:section.education
     [:div.image
      (when image
        [:img {:src (inline-image (str "images/" image))
               :alt name}])]
     [:div.text
      [:h3
       (if url
         [:a {:href url} name]
         name)]
      [:p diploma]]]))

(defn render-resume
  "Publish the resume as HTML"
  [{:keys [resume/name resume/tagline resume/notable-affiliations resume/location resume/specialties
           resume/goal resume/experience resume/education] :as resume}]
  [:html
   [:head
    [:meta {:charset "UTF-8"}]
    [:title name]
    [:style {:type "text/css"}
     (slurp (io/resource "style.css"))]]
   [:body
    [:article
     [:header
      [:h1 name]
      [:p.tagline tagline]
      [:p.notable-affiliations  (str/join " • " notable-affiliations)]
      [:p.location
       location]
      [:p.specialties (str "Specialties: " (str/join ", " specialties))]]
     [:section
      [:h2 "Goal"]
      (map #(vector :p %) goal)]
     [:section
      [:h2 "Experience"]
      (map render-work experience)]
     [:section
      [:h2 "Education"]
      (map render-education education)]
     [:footer
      [:p (str "Last updated: " (.format (java.time.LocalDate/now)
                                         (java.time.format.DateTimeFormatter/ofPattern "MMMM YYYY")))]]]]])

(defn checked-sh
  [& args]
  (let [{:keys [exit out err] :as resp} (apply sh args)]
    (when-not (zero? exit)
      (throw (ex-info (str "Shell error: " err) resp)))
    out))

(defn publish
  []
  (let [resume (-> "resume.edn" io/resource slurp edn/read-string)]
    (when-not (s/valid? ::resume resume)
      (throw (ex-info "Invalid résumé"
                      {:explain-str (s/explain-str ::resume resume)})))
    (->> resume
         (s/conform ::resume)
         render-resume
         html
         (spit "index.html"))))

(comment

  (require 'com.benfle.resume :reload)
  (in-ns 'com.benfle.resume)

  (publish)

  )
