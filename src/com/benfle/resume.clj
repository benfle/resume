(ns com.benfle.resume
  "A set of functions to generate my résumé at http://benfle.com/resume"
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.data.codec.base64 :as base64]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [doctype]]
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
(s/def :work/period (s/cat :start string?
                           :end string?))
(s/def :work/title string?)
(s/def :work/affiliation ::affiliation)
(s/def :work/projects (s/coll-of string?))
(s/def :work/recommendations (s/coll-of ::recommendation))
(s/def ::education (s/keys :req [:education/period
                                 :education/affiliation
                                 :education/diploma]))
(s/def :education/affiliation ::affiliation)
(s/def :education/diploma string?)
(s/def :education/period :work/period)
(s/def ::resume (s/keys :req [:resume/url
                              :resume/name
                              :resume/email
                              :resume/phone
                              :resume/tagline
                              :resume/goals
                              :resume/experience
                              :resume/education]))
(s/def :resume/name string?)
(s/def :resume/email string?)
(s/def :resume/phone string?)
(s/def :resume/tagline string?)
(s/def :resume/location string?)
(s/def :resume/goals (s/coll-of string?))
(s/def :resume/experience (s/coll-of ::work))
(s/def :resume/education (s/coll-of ::education))

;; Publishing

(defn render-work
  [{:keys [work/period work/title work/projects work/affiliation work/recommendations]}]
  (let [{:keys [start end]} period
        {:keys [affiliation/name affiliation/url affiliation/image]} affiliation]
    [:section.subsection
     [:h3
      [:span.affiliation
       (if url
         [:a {:href url} name]
         name)]
      ", "
      [:span.title
       title]
      [:span.period
       (str start " — " end)]]
     (map (fn [description]
            [:p description])
          projects)]))

(defn render-education
  [{:keys [education/affiliation education/diploma education/period]}]
  (let [{:keys [start end]} period
        {:keys [affiliation/name affiliation/url affiliation/image]} affiliation]
    [:section.subsection
     [:h3
      [:span.affiliation
       (if url
         [:a {:href url} name]
         name)]
      [:span.period
       (str start " — " end)]]
     [:p diploma]]))

(defn render-resume
  "Publish the resume as HTML"
  [{:keys [resume/url
           resume/name
           resume/email
           resume/phone
           resume/tagline
           resume/location
           resume/goals
           resume/experience
           resume/education] :as resume}]
  [:html {:lang "en"}
   [:head
    [:meta {:charset "UTF-8"}]
    [:meta {:name "viewport"
            :content "width=device-width, initial-scale=1"}]
    [:title name]
    [:style
     (slurp (io/resource "style.css"))]]
   [:body
    [:article
     [:header
      [:h1 name]
      [:p.tagline tagline]
      [:p.email
       email]
      [:p.phone
       phone]]
     [:section
      [:h2 "Goals"]
      (map #(vector :p %) goals)]
     [:section
      [:h2 "Experience"]
      (map render-work experience)]
     [:section
      [:h2 "Education"]
      (map render-education education)]
     [:footer
      [:p (str "Version: " (.format (java.time.LocalDate/now)
                                    (java.time.format.DateTimeFormatter/ofPattern "MMMM YYYY")))]
      [:p (str "Latest version at: " url)]]]]])

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
         (html (doctype :html5))
         (spit "index.html"))))

(comment

  (require 'com.benfle.resume :reload)

  (com.benfle.resume/publish)

  )
