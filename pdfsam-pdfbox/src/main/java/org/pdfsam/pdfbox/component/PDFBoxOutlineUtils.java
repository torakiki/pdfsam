/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mar/2015
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.pdfbox.component;

import java.io.IOException;
import java.util.Optional;

import org.apache.pdfbox.pdmodel.PDDestinationNameTreeNode;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentNameDictionary;
import org.apache.pdfbox.pdmodel.interactive.action.PDAction;
import org.apache.pdfbox.pdmodel.interactive.action.PDActionGoTo;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.PDDestination;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.PDNamedDestination;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.PDPageDestination;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineItem;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility methods related to outline handling in PDFBox
 * 
 * @author Andrea Vacondio
 *
 */
public final class PDFBoxOutlineUtils {

    private static final Logger LOG = LoggerFactory.getLogger(PDFBoxOutlineUtils.class);

    private PDFBoxOutlineUtils() {
        // utility
    }

    /**
     * @param document
     * @return the max bookmarks level where a page destination (page destination, named destination, goto action) is defined.
     */
    public static int getMaxBookmarkLevel(PDDocument document) {
        PDDestinationNameTreeNode destinations = null;
        PDDocumentNameDictionary names = document.getDocumentCatalog().getNames();
        if (names != null) {
            destinations = names.getDests();
        }
        return getMaxBookmarkLevel(document.getDocumentCatalog().getDocumentOutline(), destinations, 0);
    }

    private static int getMaxBookmarkLevel(PDOutlineNode node, PDDestinationNameTreeNode destinations, int parentLevel) {
        int maxLevel = parentLevel;
        if (node != null) {
            for (PDOutlineItem current : node.children()) {
                if (isPageDestination(current, destinations)) {
                    int maxBookmarkBranchLevel = getMaxBookmarkLevel(current, destinations, parentLevel + 1);
                    if (maxBookmarkBranchLevel > maxLevel) {
                        maxLevel = maxBookmarkBranchLevel;
                    }
                }
            }
        }
        return maxLevel;
    }

    /**
     * @param current
     *            the outline item
     * @param destinations
     *            the named destinations tree to look for in case of {@link PDNamedDestination}
     * @return the {@link PDPageDestination} for the given {@link PDOutlineItem} or null if the destination is not a page. In case the outline item has a named destination, it is
     *         resolved against the given names tree.
     */
    public static Optional<PDPageDestination> toPageDestination(PDOutlineItem current,
            PDDestinationNameTreeNode destinations) {
        try {
            PDDestination dest = current.getDestination();
            if (dest == null) {
                PDAction outlineAction = current.getAction();
                if (outlineAction instanceof PDActionGoTo) {
                    dest = ((PDActionGoTo) outlineAction).getDestination();
                }
            }
            if (dest instanceof PDNamedDestination && destinations != null) {
                dest = (PDDestination) destinations.getValue(((PDNamedDestination) dest).getNamedDestination());
            }
            if (dest instanceof PDPageDestination) {
                return Optional.of((PDPageDestination) dest);
            }
        } catch (IOException e) {
            LOG.warn("Unable to get outline item destination ", e);
        }
        return Optional.empty();
    }

    private static boolean isPageDestination(PDOutlineItem current, PDDestinationNameTreeNode destinations) {
        return toPageDestination(current, destinations).isPresent();
    }

    /**
     * Copies the dictionary from the given {@link PDOutlineItem} to the destination one
     * 
     * @param from
     * @param to
     */
    public static void copyOutlineDictionary(PDOutlineItem from, PDOutlineItem to) {
        to.setTitle(from.getTitle());
        to.setTextColor(from.getTextColor());
        to.setBold(from.isBold());
        to.setItalic(from.isItalic());
        if (from.isNodeOpen()) {
            to.openNode();
        } else {
            to.closeNode();
        }
    }
}
