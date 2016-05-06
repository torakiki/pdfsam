/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.io;

import static java.util.Arrays.asList;
import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.pdfsam.ui.help.HelpUtils.helpIcon;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.commons.SetDestinationRequest;
import org.pdfsam.ui.io.PdfVersionCombo.DefaultPdfVersionComboItem;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.parameter.base.AbstractPdfOutputParameters;
import org.sejda.model.pdf.PdfVersion;

import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
/**
 * Panel letting the user select an output destination for generated Pdf document/s.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfDestinationPane extends DestinationPane implements ModuleOwned, RestorableView,
        TaskParametersBuildStep<AbstractPdfOutputParametersBuilder<? extends AbstractPdfOutputParameters>> {

    private PdfVersionCombo version;
    private PdfVersionConstrainedCheckBox compress;
    private Optional<CheckBox> discardBookmarks = empty();
    private String ownerModule = StringUtils.EMPTY;
    private UserContext userContext;

    public PdfDestinationPane(BrowsableField destination, String ownerModule, UserContext userContext,
            DestinationPanelFields... optionalFields) {
        super(destination);
        destination.setId(ownerModule + ".destination");
        requireNotNull(userContext, "UserContext cannot be null");
        this.userContext = userContext;
        this.ownerModule = defaultString(ownerModule);
        version = new PdfVersionCombo(ownerModule);
        compress = new PdfVersionConstrainedCheckBox(PdfVersion.VERSION_1_5, ownerModule);
        compress.setText(DefaultI18nContext.getInstance().i18n("Compress output file/files"));
        if (asList(optionalFields).contains(DestinationPanelFields.DISCARD_BOOKMARKS)) {
            CheckBox discardBookmarksField = new CheckBox(DefaultI18nContext.getInstance().i18n("Discard bookmarks"));
            discardBookmarksField.setGraphic(helpIcon(DefaultI18nContext.getInstance()
                    .i18n("Tick the box if you don't want to retain any bookmark from the original PDF document")));
            discardBookmarksField.getStyleClass().addAll(Style.WITH_HELP.css());
            discardBookmarksField.setId("discardBookmarksField");
            discardBookmarks = Optional.of(discardBookmarksField);
        }
        HBox versionPane = new HBox(new Label(DefaultI18nContext.getInstance().i18n("Output PDF version:")), version);
        versionPane.getStyleClass().add("both-spaced-vitem");
        versionPane.getStyleClass().addAll(Style.HCONTAINER.css());
        getChildren().add(compress);
        discardBookmarks.ifPresent(getChildren()::add);
        getChildren().add(versionPane);
        eventStudio().addAnnotatedListeners(this);
    }

    public void enableSameAsSourceItem() {
        version.enableSameAsSourceItem();
    }

    @EventStation
    public String getOwnerModule() {
        return ownerModule;
    }

    @EventListener
    public void setDestination(SetDestinationRequest event) {
        if (!event.isFallback()
                || (isBlank(destination().getTextField().getText()) && userContext.isUseSmartOutput())) {
            destination().setTextFromFile(event.getFootprint());
        }
    }

    public void apply(AbstractPdfOutputParametersBuilder<? extends AbstractPdfOutputParameters> builder,
            Consumer<String> onError) {
        builder.compress(compress.isSelected());
        if (overwrite().isSelected()) {
            builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
        }
        builder.version(version.getSelectionModel().getSelectedItem().getVersion());
        discardBookmarks.ifPresent(d -> {
            builder.discardBookmarks(d.isSelected());
        });
    }

    public void saveStateTo(Map<String, String> data) {
        data.put("compress", Boolean.toString(compress.isSelected()));
        data.put("overwrite", Boolean.toString(overwrite().isSelected()));
        discardBookmarks.ifPresent(d -> {
            data.put("discardBookmarks", Boolean.toString(d.isSelected()));
        });
        data.put("version", version.getSelectionModel().getSelectedItem().getVersion().toString());
    }

    public void restoreStateFrom(Map<String, String> data) {
        version.initializeState();
        compress.setSelected(Boolean.valueOf(data.get("compress")));
        overwrite().setSelected(Boolean.valueOf(data.get("overwrite")));
        discardBookmarks.ifPresent(d -> {
            d.setSelected(Boolean.valueOf(data.get("discardBookmarks")));
        });
        ofNullable(data.get("version")).map(PdfVersion::valueOf).map(DefaultPdfVersionComboItem::new)
                .ifPresent(v -> this.version.getSelectionModel().select(v));
    }

    public static enum DestinationPanelFields {
        DISCARD_BOOKMARKS;
    }
}
