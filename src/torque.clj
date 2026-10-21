(ns torque
  (:require
   [clojure.string :as str]
   [ring.util.codec])
  (:import
   [java.util.zip GZIPOutputStream]
   [java.io ByteArrayOutputStream]
   [com.aayushatharva.brotli4j Brotli4jLoader]
   [com.aayushatharva.brotli4j.encoder BrotliOutputStream Encoder$Parameters]))

;; ── compression ───────────────────────────────────────────────────────────

(Brotli4jLoader/ensureAvailability)

(defn gzip-encoder
  "Stateful fn that gzip-compresses each string with SYNC_FLUSH so receivers
   get bytes per call. Not thread-safe — one per channel."
  []
  (let [baos (ByteArrayOutputStream.)
        gz (GZIPOutputStream. baos true)]
    (fn [^String s]
      (.write gz (.getBytes s "UTF-8"))
      (.flush gz)
      (let [out (.toByteArray baos)]
        (.reset baos)
        out))))

(defn brotli-encoder
  "Stateful fn that brotli-compresses each string, flushing after every call.
   Not thread-safe — one per channel."
  []
  (let [baos (ByteArrayOutputStream.)
        br (BrotliOutputStream. baos (.setQuality (Encoder$Parameters.) 6))]
    (fn [^String s]
      (.write br (.getBytes s "UTF-8"))
      (.flush br)
      (let [out (.toByteArray baos)]
        (.reset baos)
        out))))

(defn pick-encoder [req]
  (let [accept-encoding (str (get-in req [:headers "accept-encoding"]))]
    (cond
      (str/includes? accept-encoding "br")
      {:headers {"Content-Encoding" "br"} :encode (brotli-encoder)}
      (str/includes? accept-encoding "gzip")
      {:headers {"Content-Encoding" "gzip"} :encode (gzip-encoder)}
      :else
      {:headers {} :encode (fn [^String s] (.getBytes s "UTF-8"))})))

;; ── commands ──────────────────────────────────────────────────────────────

(defn invoke
  "Create a Datastar expression to invoke the given command

  Invoke the #'increase command on click:

      {:data-on:click (invoke #'increase)}

  Invoke the #'plus command with parameter delta=5:

      {:data-on:click (invoke #'plus {\"delta\" 5})}"
  ([command-var]
   (str "@post('" (-> command-var meta :command/uri) "')"))
  ([command-var params]
   (str "@post('" (-> command-var meta :command/uri)
        "?" (ring.util.codec/form-encode params) "')")))

;; ── draft ─────────────────────────────────────────────────────────────────

(comment
  )
