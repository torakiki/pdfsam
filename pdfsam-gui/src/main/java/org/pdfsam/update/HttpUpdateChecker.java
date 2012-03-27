/*
 * Created on 26-Feb-2008
 * Copyright (C) 2008 by Andrea Vacondio.
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.update;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import static org.pdfsam.support.RequireUtils.require;
import static org.pdfsam.support.XmlUtils.nullSafeGetStringAttribute;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultIfBlank;

/**
 * Checks for update over a http connection
 * 
 * @author Andrea Vacondio
 * 
 */
class HttpUpdateChecker implements UpdateChecker {

    private static final Logger LOG = LoggerFactory.getLogger(HttpUpdateChecker.class);

    private static final String VERSION_ATTRIBUTE = "value";
    private static final String XPATH_VERSION_NODE = "/pdfsam/latestVersion";

    private String uri = null;

    HttpUpdateChecker(String uri) {
        require(StringUtils.isNotBlank(uri), "URI to check cannot be blank");
        this.uri = uri;
    }

    private String parseXmlStream(InputStream is) throws ParserConfigurationException, SAXException, IOException,
            XPathExpressionException {
        DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document document = builder.parse(is);
        Node node = (Node) XPathFactory.newInstance().newXPath()
                .evaluate(XPATH_VERSION_NODE, document, XPathConstants.NODE);
        return defaultIfBlank(nullSafeGetStringAttribute(node, VERSION_ATTRIBUTE), EMPTY);
    }

    public String getLatestVersion() {
        HttpURLConnection urlConn = null;
        InputStream is = null;
        try {
            URL url = new URL(uri);
            urlConn = (HttpURLConnection) url.openConnection();
            urlConn.setRequestProperty("user agent", "Mozilla/5.0 (compatible; MSIE 6.0; Windows NT 5.1");
            is = urlConn.getInputStream();
            return parseXmlStream(is);
        } catch (Exception e) {
            LOG.warn(DefaultI18nContext.getInstance().getI18n().tr("Unable to get latest available version"), e);
        } finally {
            if (urlConn != null) {
                urlConn.disconnect();
            }
            IOUtils.closeQuietly(is);
        }
        return EMPTY;
    }
}
