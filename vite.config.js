import { defineConfig } from "vite";
import { plugin as elmPlugin } from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
});
