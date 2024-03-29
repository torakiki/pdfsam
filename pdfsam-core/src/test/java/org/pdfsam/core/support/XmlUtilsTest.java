/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/dic/2011
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.core.support;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Andrea Vacondio
 * 
 */
public class XmlUtilsTest {
    private Document doc;

    @BeforeEach
    public void setUp() throws SAXException, IOException, ParserConfigurationException {
        DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
        domFactory.setNamespaceAware(true);
        DocumentBuilder builder = domFactory.newDocumentBuilder();
        doc = builder.parse(getClass().getClassLoader().getResourceAsStream("test.xml"));
    }

    @Test
    public void nullSafeGetStringAttribute() throws XPathExpressionException {
        Node node = (Node) XPathFactory.newInstance().newXPath().evaluate("/test", doc, XPathConstants.NODE);
        assertEquals("stringValue", XmlUtils.nullSafeGetStringAttribute(node, "stringAttr"));
        assertNull(XmlUtils.nullSafeGetStringAttribute(node, "chuckNorris"));
        Node node2 = (Node) XPathFactory.newInstance().newXPath().evaluate("/chuckNorris", doc, XPathConstants.NODE);
        assertNull(node2);
        assertNull(XmlUtils.nullSafeGetStringAttribute(node2, "kick"));
    }
}
