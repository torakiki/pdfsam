/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/mar/2015
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
package org.pdfsam.pdfbox;

import static java.util.Objects.requireNonNull;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.pdfbox.component.PDFBoxOutlineUtils.getMaxBookmarkLevel;
import static org.pdfsam.pdfbox.component.PDFBoxOutlineUtils.toPageDestination;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.pdfbox.pdmodel.PDDestinationNameTreeNode;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentNameDictionary;
import org.apache.pdfbox.pdmodel.PDPageTree;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.PDPageDestination;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineItem;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineNode;
import org.sejda.model.outline.OutlineLevelsHandler;
import org.sejda.model.outline.OutlinePageDestinations;

/**
 * PDFBox implementation of an {@link OutlineLevelsHandler}
 * 
 * @author Andrea Vacondio
 *
 */
class PDFBoxOutlineLevelsHandler implements OutlineLevelsHandler {

    private Pattern titleMatchingPattern = Pattern.compile(".+");
    private PDDestinationNameTreeNode namedDestinations = null;
    private PDDocument document;
    private PDPageTree pages;

    public PDFBoxOutlineLevelsHandler(PDDocument document, String matchingTitleRegEx) {
        requireNonNull(document, "Unable to retrieve bookmarks from a null document.");
        this.document = document;
        this.pages = document.getPages();
        PDDocumentNameDictionary names = document.getDocumentCatalog().getNames();
        if (names != null) {
            this.namedDestinations = names.getDests();
        }
        if (isNotBlank(matchingTitleRegEx)) {
            this.titleMatchingPattern = Pattern.compile(matchingTitleRegEx);
        }
    }

    public int getMaxOutlineDepth() {
        return getMaxBookmarkLevel(document);
    }

    public OutlinePageDestinations getPageDestinationsForLevel(int level) {
        OutlinePageDestinations destinations = new OutlinePageDestinations();
        addPageIfBookmarkLevel(document.getDocumentCatalog().getDocumentOutline(), 1, destinations, level);
        return destinations;
    }

    private void addPageIfBookmarkLevel(PDOutlineNode outline, int currentLevel, OutlinePageDestinations destinations,
            int levelToAdd) {
        if (outline != null) {
            for (PDOutlineItem current : outline.children())
                if (currentLevel <= levelToAdd) {
                    toPageDestination(current, namedDestinations).ifPresent(d -> {
                        if (isLevelToBeAdded(currentLevel, levelToAdd)) {
                            addPageIfValid(destinations, d, current.getTitle());
                        } else {
                            addPageIfBookmarkLevel(current, currentLevel + 1, destinations, levelToAdd);
                        }
                    });
                }
        }
    }

    private boolean isLevelToBeAdded(int currentLevel, int levelToAdd) {
        return currentLevel == levelToAdd;
    }

    private void addPageIfValid(OutlinePageDestinations destinations, PDPageDestination destination, String title) {
        if (isNotBlank(title)) {
            Matcher matcher = titleMatchingPattern.matcher(title);
            if (matcher.matches()) {
                destinations.addPage(pages.indexOf(destination.getPage()), title);
            }
        }
    }
}
