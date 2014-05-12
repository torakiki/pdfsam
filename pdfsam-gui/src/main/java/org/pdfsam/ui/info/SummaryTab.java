/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/mag/2014
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
package org.pdfsam.ui.info;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.text.DateFormat;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.WeakChangeListener;
import javafx.geometry.HPos;
import javafx.geometry.Pos;
import javafx.geometry.VPos;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Tab;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;

import javax.annotation.PostConstruct;
import javax.inject.Named;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.event.ShowPdfDescriptorRequest;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.model.pdf.PdfMetadataKey;

import com.itextpdf.text.pdf.PdfDate;

/**
 * Tab displaying a summary of the PDF document information.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class SummaryTab extends Tab {
    private static FastDateFormat FORMATTER = FastDateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.MEDIUM);
    private GridPane grid = new GridPane();
    private Label file = createValueLabel();
    private Label size = createValueLabel();
    private Label version = createValueLabel();
    private Label created = createValueLabel();
    private Label modified = createValueLabel();
    private Label pages = createValueLabel();
    private Label title = createValueLabel();
    private Label author = createValueLabel();
    private Label creator = createValueLabel();
    private Label producer = createValueLabel();
    private Label subject = createValueLabel();
    private ChangeListener<Boolean> loadedListener;

    SummaryTab() {
        setText(DefaultI18nContext.getInstance().i18n("Summary"));
        setClosable(false);
        grid.getStyleClass().add("info-props");
        grid.setAlignment(Pos.TOP_CENTER);
        ColumnConstraints column1 = new ColumnConstraints();
        column1.setPercentWidth(25);
        ColumnConstraints column2 = new ColumnConstraints();
        column2.setPercentWidth(75);
        grid.getColumnConstraints().addAll(column1, column2);
        ScrollPane scroll = new ScrollPane(grid);
        scroll.setFitToHeight(true);
        scroll.setFitToWidth(true);
        setContent(scroll);
    }

    @PostConstruct
    void init() {
        grid.add(createTitleLabel("File"), 0, 0);
        grid.add(file, 1, 0);
        grid.add(createTitleLabel("Size"), 0, 1);
        grid.add(size, 1, 1);
        grid.add(createTitleLabel("Created"), 0, 2);
        grid.add(created, 1, 2);
        grid.add(createTitleLabel("Modified"), 0, 3);
        grid.add(modified, 1, 3);
        grid.add(createTitleLabel("PDF version"), 0, 4);
        grid.add(version, 1, 4);
        grid.add(createTitleLabel("Pages"), 0, 5);
        grid.add(pages, 1, 5);
        grid.add(createTitleLabel("Title"), 0, 6);
        grid.add(title, 1, 6);
        grid.add(createTitleLabel("Author"), 0, 7);
        grid.add(author, 1, 7);
        grid.add(createTitleLabel("Creator"), 0, 8);
        grid.add(creator, 1, 8);
        grid.add(createTitleLabel("Producer"), 0, 9);
        grid.add(producer, 1, 9);
        grid.add(createTitleLabel("Subject"), 0, 10);
        grid.add(subject, 1, 10);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    void requestShow(ShowPdfDescriptorRequest event) {
        loadedListener = (o, oldVal, newVal) -> {
            Platform.runLater(() -> {
                version.setText(event.getDescriptor().getVersion());
                pages.setText(Integer.toString(event.getDescriptor().pagesPropery().get()));
                created.setText(FORMATTER.format(PdfDate.decode(event.getDescriptor().getInformation("CreationDate"))));
                title.setText(event.getDescriptor().getInformation(PdfMetadataKey.TITLE.getKey()));
                author.setText(event.getDescriptor().getInformation(PdfMetadataKey.AUTHOR.getKey()));
                creator.setText(event.getDescriptor().getInformation(PdfMetadataKey.CREATOR.getKey()));
                subject.setText(event.getDescriptor().getInformation(PdfMetadataKey.SUBJECT.getKey()));
                producer.setText(event.getDescriptor().getInformation("Producer"));
            });

        };
        event.getDescriptor().loadedProperty().addListener(new WeakChangeListener<>(loadedListener));
        file.setText(event.getDescriptor().getFile().getAbsolutePath());
        size.setText(FileUtils.byteCountToDisplaySize(event.getDescriptor().getFile().length()));
        created.setText(FORMATTER.format(PdfDate.decode(event.getDescriptor().getInformation("CreationDate"))));
        modified.setText(FORMATTER.format(event.getDescriptor().getFile().lastModified()));
        version.setText(event.getDescriptor().getVersion());
        pages.setText(Integer.toString(event.getDescriptor().pagesPropery().get()));
        title.setText(event.getDescriptor().getInformation(PdfMetadataKey.TITLE.getKey()));
        author.setText(event.getDescriptor().getInformation(PdfMetadataKey.AUTHOR.getKey()));
        creator.setText(event.getDescriptor().getInformation(PdfMetadataKey.CREATOR.getKey()));
        producer.setText(event.getDescriptor().getInformation("Producer"));
        subject.setText(event.getDescriptor().getInformation(PdfMetadataKey.SUBJECT.getKey()));
    }

    private static Label createTitleLabel(String text) {
        Label ret = new Label(DefaultI18nContext.getInstance().i18n(text) + ":");
        ret.getStyleClass().add("info-property");
        GridPane.setHalignment(ret, HPos.RIGHT);
        GridPane.setValignment(ret, VPos.TOP);
        return ret;
    }

    private static Label createValueLabel() {
        Label ret = new Label();
        ret.getStyleClass().add("info-property-value");
        ret.setWrapText(true);
        return ret;
    }
}
