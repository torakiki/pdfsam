/*
 * Created on 13/dic/2011
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.support;

import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * @author Andrea Vacondio
 * 
 */
public class XmlUtilsTest {
    private Document doc;

    @Before
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
