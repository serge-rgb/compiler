/**
 * Emit html for each instruction. Click on an instruction to see information
 * about how it was generated.
 */

void
htmlBegin(Html* html) {
   html->fd = fopen("out.html", "w");
   char* out =
           "<html>"
           "<head>"
           "<script>"
           "function show(showName) {\n"
           "    document.getElementById(showName).removeAttribute('hidden');\n"
           "}\n"
           "</script>"
           "<style>"
           "p {"
           "font-family: \"Lucida Console\", Monaco, monospace;"
           "}"
           ".hidden {"
           "color: blue;"
           "}"
           "</style>"
           "</head>"
           "<body>";

   fwrite(out, 1, strlen(out), html->fd);
   Assert(html->fd);
}

void
htmlEmit(Html* html, char* str, char* hidden) {
   char instr[128] = {0};
   snprintf(instr, 128, "instr%d", html->instr_count++);
   char out[1024] = {0};
   snprintf(out, 1024,
           "<p onclick=\"show('%s')\">%d: %s</p>"
           "<p id='%s' class='hidden' hidden> ---- %s </p>",
            instr, html->instr_count, str, instr, hidden);
   fwrite(out, 1, strlen(out), html->fd);
}

void
htmlEnd(Html* html) {
   char* out =
           "</body></html>";
   fwrite(out, 1 ,strlen(out), html->fd);
   fclose(html->fd);
}
