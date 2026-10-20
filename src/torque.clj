(ns torque
  (:require
   [clojure.string :as str])
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

;; ── draft ─────────────────────────────────────────────────────────────────

(comment
  ;; Torque needs the system atom.
  torque/!system
  :torque.conn/conn
  :torque.conn/encoder
  :torque/connections)

(comment
  ;; How do we want to construct commands?

  (defn create-book-expr [path day hour]
    (str "@post('" path "?"
         (ring.util.codec/form-encode {:day day :hour hour})
         "')"))
  [:button {:data-on:click (create-book-expr "/book-appointment" day hour)} "bk"]
  ;; introduces the need to write helpers.

  ;; how about
  [:button (cmd :on/click :commands/book-appointment {"day" day "hour" hour}) "bk"]
  ;; ?
  )
