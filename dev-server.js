import * as https from "node:https";
import * as http from "node:http";
import elmWatch from "elm-watch";

try {
    const exitCode = await elmWatch(["hot"], {
        createServer: ({ onRequest, onUpgrade }) =>
            http
                .createServer((request, response) => {
                    if (
                        request.url.startsWith("/it") ||
                        request.url.startsWith("/services/timetables")
                    ) {
                        tplFvgProxy(request, response);
                    } else {
                        // Let elm-watch’s server do its thing for all other URLs.
                        onRequest(request, response);
                    }
                })
                .on("upgrade", onUpgrade),
    });
    process.exit(exitCode);
} catch (error) {
    console.error("Unexpected elm-watch error:", error);
}

function tplFvgProxy(request, response) {
    const options = {
        hostname: "tplfvg.it",
        port: 443,
        path: request.url,
        method: request.method,
        headers: request.headers,
    };
    options.headers.host = "tplfvg.it:443";

    const proxyRequest = https.request(options, (proxyResponse) => {
        response.writeHead(proxyResponse.statusCode, proxyResponse.headers);
        proxyResponse.pipe(response, { end: true });
    });

    proxyRequest.on("error", (error) => {
        response.writeHead(503);
        response.end(`Failed to proxy to tplfvg.it.\n\n${error.stack}`);
    });

    request.pipe(proxyRequest, { end: true });
}
