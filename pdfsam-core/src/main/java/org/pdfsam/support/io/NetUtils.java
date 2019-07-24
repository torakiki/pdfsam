/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24 lug 2019
 * Copyright 2017 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.support.io;

import static java.util.Objects.isNull;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Andrea Vacondio
 *
 */
public class NetUtils {

    /**
     * https://stackoverflow.com/questions/1884230/urlconnection-doesnt-follow-redirect
     * 
     * @param url
     * @return
     * @throws IOException
     */
    public static InputStream urlToStream(URL url) throws IOException {
        Map<String, Integer> visited = new HashMap<>();
        
        while (true) {

            if (visited.compute(url.toExternalForm(), (key, count) -> isNull(count) ? 1 : count++) > 3) {
                throw new IOException("Too many redirects");
            }

            HttpURLConnection connection = (HttpURLConnection) url.openConnection();

            connection.setConnectTimeout(15000);
            connection.setReadTimeout(15000);
            connection.setInstanceFollowRedirects(false); // Make the logic below easier to detect redirections
            connection.setRequestProperty("User-Agent", "Mozilla/5.0 (PDFsam Basic)");

            switch (connection.getResponseCode()) {
            case HttpURLConnection.HTTP_MOVED_PERM:
            case HttpURLConnection.HTTP_MOVED_TEMP:
                String location = connection.getHeaderField("Location");
                location = URLDecoder.decode(location, "UTF-8");
                url = new URL(url, location); // Deal with relative URLs
                connection.disconnect();
                continue;
            }
            break;
        }
        return url.openStream();
    }
}
